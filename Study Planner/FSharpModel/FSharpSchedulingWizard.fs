module FSharpSchedulingWizard

open QUT
open StudyPlannerModel

// Functions used for optimizing study plan ...

// The semester that we are currently in
let currentSemester : Semester = 
    // Do not change
    { year = 2020; offering = Semester1 }


// Given a partial plan, try to schedule the remaining units such that all remaining units are legally scheduled 
// (with no more than 4 units per semester).
let rec private scheduleRemaining (remainingUnits:BoundPlan) (plannedUnits:StudyPlan): StudyPlan option =
    match remainingUnits with 
    | [] -> Some plannedUnits // if no remaining units, then we have a complete study plan
    | _ -> 

        /// try and find an enrollable subject
        let enrollable = 
            remainingUnits 
            |> Seq.tryFind (fun subject -> 
                subject.possibleSemesters 
                |> Seq.exists (fun sem -> 
                    isEnrollableIn subject.code sem plannedUnits))
        
        match enrollable with
        | None -> Option.None // if no subject exists, nothing is possible
        | Some (subject) ->
            subject.possibleSemesters 
            |> Seq.filter (fun sem -> isEnrollableIn subject.code sem plannedUnits) // find the enrollable semesters
            |> Seq.map (fun sem -> 
                // create new study plan with the available subject + semester
                let newPlan = 
                    plannedUnits 
                    |> Seq.append (seq [{code = subject.code; studyArea = subject.studyArea; semester = sem}])

                // create new bound without the added subject
                let newRemaining =
                    remainingUnits
                    |> Seq.filter (fun plannedUnit -> not (plannedUnit.code = subject.code)) |> Seq.toList
                
                scheduleRemaining newRemaining newPlan) 
            |> Seq.tryFind Option.isSome // will produce option option types
            |> Option.flatten // reduce option option type to option


// Assuming that study commences in the given first semester and that units are only studied 
// in semester 1 or semester 2, returns the earliest possible semester by which all units in
// the study plan could be completed, assuming at most 4 units per semester.
let private bestAchievable (firstSemester:Semester) (plan:StudyPlan) : Semester =
      let length = plan |> Seq.length 
      let semesters = (ceil) ((float) length / 4.0) |>  int // number of semesters required to complete all subjects in plan
      let years = semesters / 2 // 2 semesters per year
  
      match firstSemester.offering with
      | Semester1 when semesters % 2 = 1 -> {year = firstSemester.year + years; offering = Semester1}
      | Semester1 -> {year = firstSemester.year + years - 1; offering = Semester2}
      | Semester2 when semesters % 2 = 1 -> { year = firstSemester.year + years; offering = Semester2 }
      | Semester2 -> {year = firstSemester.year + years; offering = Semester1}
      | _ -> firstSemester /// will go unmatched


// Returns the last semester in which units will be studied in the study plan
let lastSemester (plan: StudyPlan): Semester =
     plan 
     |> Seq.map (fun unitInPlan -> unitInPlan.semester) // get each semester subjects in plan will be studied in
     |> Seq.max 

     
// Returns true if and only if every unit in the plan has at least one possible semester for it to be scheduled
let allBoundsFeasible (bounds:BoundPlan) =
    // do not change  (difficulty: 3/10)
    bounds |> Seq.forall (fun unit -> not (Seq.isEmpty unit.possibleSemesters)) 



// Returns a sequence of progressively better study plans.
let TryToImproveSchedule (plan:StudyPlan) : seq<StudyPlan> =
    let first = currentSemester // earliest semester
    let last = lastSemester plan /// latest semester
    let bestPossible = bestAchievable first plan // lower bound
    
    let rec TryToCompleteBy (targetGraduation:Semester) =
        if (targetGraduation < bestPossible) then // if target is before best possible then nothing can be done
            Seq.empty
        else
                let bound = BoundsOptimizer.boundUnitsInPlan plan first targetGraduation // find bounds of each subject in plan
                if not (allBoundsFeasible bound) then 
                    Seq.empty // can't complete plan if there is a subject with infeasible bounds

                else 
                    let newPlan = scheduleRemaining bound Seq.empty // reschedule new plan 
                    match newPlan with 
                    | Some (studyPlan) -> 
                        // if there is a possible plan, begin creating sequence of study plans
                        seq {
                            yield studyPlan
                            yield! TryToCompleteBy (previousSemester (lastSemester studyPlan))
                        }
                    | None -> Seq.empty // if no plan is possible, then return empty sequence
            
    TryToCompleteBy (previousSemester last)
