module BoundsOptimizer

open QUT
open StudyPlannerModel 

// Returns a sequence of all pairs of units (X,Y) such that X and Y are both part of the study plan and X must be completed before Y.
// if (IsEnrollable prereq studyplan w/o prereq is false) then (X, Y)
let unitDependenciesWithinPlan (allUnits: StudyPlan) : seq<UnitCode*UnitCode> =
    seq {
        for unitInPlan in allUnits do
            for prereq in UnitPrereqs unitInPlan.code do // for all prereqs in the current study plan
                if (allUnits |> Seq.exists (fun subject -> subject.code = prereq)) then // if a subject has a prereq listed
                    if not (isEnrollable unitInPlan.code (allUnits |> Seq.filter (fun subject -> not (subject.code = prereq)))) then // and the subject not enrollable without first completing that prereq
                        yield (prereq, unitInPlan.code) // we have a dependency
    }
    
// Returns the first semester on or after the given semester in which the specified unit is offered
let rec private firstOfferingOnOrAfter (unitCode: UnitCode) (semester: Semester) : Semester =
   let offered = (lookup unitCode).offered |> Set.contains semester.offering // find offerings
   if offered then semester else firstOfferingOnOrAfter unitCode (nextSemester semester) // increment semester until we find a semester where the unit is offered

// Returns the first semester on or before the given semester in which the specified unit is offered
let rec private firstOfferingOnOrBefore (unitCode: UnitCode) (semester: Semester) : Semester =
    let offered = (lookup unitCode).offered |> Set.contains semester.offering
    if offered then semester else firstOfferingOnOrBefore unitCode (previousSemester semester)

// Based on a set of dependencies between units, determine the earliest possible semester in which the given unit could be studied
// assuming that all units involved in the dependencies must all be completed no earlier than the first semester.
let rec private earliestSemester (dependencies: seq<UnitCode*UnitCode>) (unitCode: UnitCode) (firstSemester: Semester)  : Semester =

    // if a subject has no dependencies, then return first semester it is offered in
    if not (dependencies |> Seq.exists (fun (_, subject) -> subject = unitCode)) then
        firstOfferingOnOrAfter unitCode firstSemester

    // if subject has dependencies, find earliest semester those dependencies are completed in
    else 
        let immediateDependencies = 
            dependencies
            |> Seq.filter (fun (_, subject) -> subject = unitCode) // find any dependencies of the given subject
            |> Seq.map ( fun (prereq, _) ->  prereq) // get all prerequisites
        
        let getSems =
            immediateDependencies 
            |> Seq.map (fun prereq -> 
                earliestSemester dependencies prereq firstSemester // find earliest semesters of prerequisite can be studied in
                |> nextSemester 
                |> firstOfferingOnOrAfter unitCode) // and find first offering of the given subject after prereq has been completed

        getSems 
        |> Seq.max // get the latest of all semesters to ensure all prerequisites have been done
    
// Based on a set of dependencies between units, determine the latest possible semester in which the given unit could be studied
// assuming that all units involved in the dependencies must all be completed no later than the last semester.
let rec private latestSemester (dependencies: seq<UnitCode*UnitCode>) (unitCode: UnitCode) (lastSemester: Semester) : Semester =
    if not (dependencies |> Seq.exists (fun (prereq, _) -> prereq = unitCode)) then 
        firstOfferingOnOrBefore unitCode lastSemester // if unitCode is not a prerequisite for anything then it can be completed as late as possible
  
    else
        let depSems = 
            dependencies
            |> Seq.filter (fun (prereq, _) -> prereq = unitCode) // find subjects that unitCode is a prereq for
            |> Seq.map (fun (_, subject) ->
                latestSemester dependencies subject lastSemester
                |> previousSemester
                |> firstOfferingOnOrBefore unitCode)
        depSems
        |> Seq.min // find earliest semester so there is enough time to complete all the subjects that are dependent on it

// Create a bound plan by determining for each unit in the study plan the earliest and latest possible semester 
// in which that unit could be taken. 
let boundUnitsInPlan (allUnits: StudyPlan) (firstSemester: Semester) (lastSemester: Semester) : BoundPlan =
    allUnits
    |> Seq.map (fun subject -> 
        let dependencies = unitDependenciesWithinPlan allUnits // find all dependencies for each subject
        let earliestSem = earliestSemester dependencies subject.code firstSemester // lower bound for completion
        let latestSem = latestSemester dependencies subject.code lastSemester // upper bound for completion

        let enrollableSems = 
            SemesterSequence earliestSem latestSem 
            |> Seq.filter (fun sem -> isOffered subject.code sem) // keep only the semesters that the subject is offered in

        {code = subject.code; studyArea = subject.studyArea; possibleSemesters = enrollableSems })
    |> Seq.toList
    


