using Microsoft.FSharp.Collections;
using System;
using System.Collections.Generic;
using System.Linq;

namespace QUT
{
    using StudyPlan = IEnumerable<UnitInPlan>;

    public class CSharpSchedulingWizard
    {
        public static Semester currentSemester
        {
            // Do not change
            get { return new Semester(2020, Offering.Semester1); }
        }


        public static Semester latestSem(StudyPlan plan)
        {
            if (plan is null)
            {
                return null;
            }

            Semester currentMax = currentSemester;
            foreach (UnitInPlan unitInPlan in plan)
            {
                if (unitInPlan.semester.CompareTo(currentMax) > 0)
                {
                    currentMax = unitInPlan.semester;
                }
            }

            return currentMax;

        }


        public static Semester bestPossible(StudyPlan plan, Semester firstSem)
        {
            if (plan is null)
            {
                return null;
            }

            int subjects = plan.Count();
            int semesters = (int)Math.Ceiling(subjects / 4.0); // total semesters = ceiling of # of subjects / 4
            int years = semesters / 2;

            if (firstSem.offering.IsSemester1)
            {
                if (semesters % 2 == 1) // semester stays the same
                {
                    return new Semester(firstSem.year + years, Offering.Semester1);
                }

                // change semesters as well
                return new Semester(firstSem.year + years - 1, Offering.Semester2); // years = current year + years - 1
            }
            else
            {
                if (semesters % 2 == 1)
                {
                    return new Semester(firstSem.year + years, Offering.Semester2); ;
                }
                return new Semester(firstSem.year + years, Offering.Semester1);
            }
        }


        public static StudyPlan scheduleRemaining(List<PlannedUnit> boundUnits, List<UnitInPlan> plan)
        {
            if (boundUnits.Count == 0) // all units have been scheduled, so return plan
            {
                return plan;
            }

            PlannedUnit firstUnit = boundUnits.FirstOrDefault(unit => unit.possibleSemesters
                .Any(sem => StudyPlannerModel.isEnrollableIn(unit.code, sem, plan)));

            if (firstUnit is null) // we have failed in finding an enrollable unit, so cannot complete schedule
            {
                return null;
            }

            // otherwise schedule in possible semester
            foreach (Semester sem in firstUnit.possibleSemesters)
            {
                if (StudyPlannerModel.isEnrollableIn(firstUnit.code, sem, plan))
                {
                    UnitInPlan newUnit = new UnitInPlan(firstUnit.code, firstUnit.studyArea, sem);
                    IEnumerable<UnitInPlan> newPlan = plan.Append(newUnit); // add new unit to plan
                    IEnumerable<PlannedUnit> newBound = boundUnits.Where(subject => subject.code != newUnit.code); // remove new unit from planned units
                    StudyPlan final = scheduleRemaining(newBound.ToList(), newPlan.ToList()); // try to schedule the rest of the planned units

                    // if we finish with a plan that is not null, then we hav succeeded and return plan
                    if (!(final is null))
                    {
                        return final;
                    }
                }
            }

            // otherwise no possible choices
            return null;
        }


        public static IEnumerable<StudyPlan> TryToImproveSchedule(StudyPlan plan)
        {
            List<StudyPlan> improvedPlans = new List<StudyPlan>();

            // earliest possible sem + first and last sem of study plan
            Semester earliestPossible = bestPossible(plan, currentSemester);
            Semester firstSem = currentSemester; /// 
            Semester lastSem = latestSem(plan);

            // get target completion date
            Semester target = StudyPlannerModel.previousSemester(lastSem); //

            /// create new bounds with target completion to be rescheduled into a new study plan 
            List<PlannedUnit> newBound = BoundsOptimizer.boundUnitsInPlan(plan, firstSem, target).ToList();
            StudyPlan newPlan;

            while (target.CompareTo(earliestPossible) >= 0) // while target is still possible
            {
                // if bounds infeasible then can't reschedule
                if (!FSharpSchedulingWizard.allBoundsFeasible(ListModule.OfSeq(newBound)))
                {
                    return improvedPlans; /// return sequence
                }
                // otherwise, add plan to improved plans
                newPlan = scheduleRemaining(newBound, new List<UnitInPlan>());
                if (newPlan is null)
                {
                    break; // stop trying to find better plans
                }

                improvedPlans.Add(newPlan);

                // update target semester
                target = StudyPlannerModel.previousSemester(latestSem(newPlan));

                // update new bounds for 1 sem earlier
                newBound = BoundsOptimizer.boundUnitsInPlan(newPlan, firstSem, target).ToList();
            }


            return improvedPlans;

        }
    }
}