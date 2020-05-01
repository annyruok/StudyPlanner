module StudyPlannerModel

open QUT

let private unitList : Map<UnitCode,UnitInfo> = 
    Parser.parseUnitData CourseData.Properties.Resources.units

// Lookup the given unit code in the unitList
let lookup (code:UnitCode) : UnitInfo = 
    unitList.[code]


// Functions dealing with semester sequences ...
// The semester prior to the given semester
let previousSemester (semester:Semester) =
    match semester.offering with
    | Semester2 -> { year = semester.year; offering = Semester1 } 
    | Summer -> { year = semester.year; offering = Semester2 }
    | Semester1 -> { year = semester.year - 1; offering = Summer }


// The semester after to the given semester
let nextSemester (semester:Semester) =
    match semester.offering with 
    | Semester1 -> {year = semester.year; offering = Semester2}
    | Semester2 -> {year = semester.year; offering = Summer}
    | Summer -> {year = semester.year + 1; offering = Semester1}

// Returns a sequence of consecutive semesters 
let rec SemesterSequence (firstSemester: Semester) (lastSemester: Semester): seq<Semester> =
    if (firstSemester <= lastSemester) then
        seq {
            yield firstSemester;
            if not (firstSemester = lastSemester) then 
                yield! SemesterSequence (nextSemester firstSemester) lastSemester
        }
    else
        Seq.empty

    
// Functions dealing with prerequisites ...    
// True if and only if the prerequisites have been met based on units in the study 
// plan taken in an earlier semester (based on the before function)
let rec private satisfied (prereq:Prereq) (plannedUnits:StudyPlan) (before: Semester->bool) : bool = 
    /// plannedUnits are pre-filtered to only contain units completed before a specified semester to avoid re-filtering
    // plannedUnits each time satisfied is called

    let totalCompleted = /// sequence of booleans containing whether prerequisites have been met 
        match prereq with 
        | Unit(unitCode) -> 
            let completed =  plannedUnits |> Seq.exists ( fun plannedUnit -> plannedUnit.code = unitCode ) // true if subject has been completed before specified semester
            seq { yield completed }
        | And(andUnits) -> // produce nested sequence for prereqs within the 'and'
            let andCompleted = 
                andUnits 
                |> Seq.forall (fun singleUnit -> satisfied singleUnit plannedUnits before) // true only if all units have been completed
            seq {yield andCompleted}
        | Or (orUnits) -> // nested sequence of prereqs within 'or'
            let orCompleted =
                orUnits 
                |> Seq.map (fun singleUnit -> satisfied singleUnit plannedUnits before) 
                |> Seq.reduce (||) // only 1 of the units needs to be completed
            seq { yield orCompleted }
        | CreditPoints(creditRequired) -> 
            let creditReached = 
                plannedUnits 
                |> Seq.sumBy (fun completed -> (lookup completed.code).creditpoints)// calculate sum of creditpoints in completedUnits
            let completed = creditReached >= creditRequired  // and then compare to creditrequired
            seq { yield completed }
        | _-> seq { true } // if no prerequisites then automatically satisfied

    totalCompleted |> Seq.forall (fun completed -> completed = true) // all sub-prerequisite components bust be evaluated to true


 // Functions used for determining when units can be studied ...
 // True if and only if the unit with the specified unit code is offered in the specified semester
let isOffered (unitCode:UnitCode) (semester:Semester) : bool = 
    (lookup unitCode).offered |> Set.exists (fun sem -> sem = semester.offering) 

// True if and only if the specified unit can be studied in the specified semester based on the specified study plan.
let isLegalIn (unitCode:UnitCode) (semester:Semester) (plannedUnits:StudyPlan) : bool =
    let filteredUnits = plannedUnits |> Seq.filter (fun planned -> planned.semester < semester) // filter out units completed after given semester from plan
    // subject must be offered and all prereqs must be satisfied
    isOffered unitCode semester 
    && satisfied (lookup unitCode).prereq filteredUnits (fun sem -> sem < semester) 


// True if and only if the specified unit can be added to the study plan in that semester.
let isEnrollableIn (unitCode:UnitCode) (semester:Semester) (plannedUnits:StudyPlan) : bool =
    plannedUnits 
    |> Seq.filter (fun plannedUnit -> plannedUnit.semester = semester ) 
    |> Seq.length < 4 // can only study 4 subjects per semester
    && isLegalIn unitCode semester plannedUnits 
    
  
// True if and only if the unit can be legally added to the study plan (in some semester) 
let isEnrollable (unitCode:UnitCode) (plannedUnits:StudyPlan) : bool =
    satisfied (lookup unitCode).prereq plannedUnits (fun sem -> true) // no semester limitations

// True if and only if the all of the units in the study plan are legally scheduled
let isLegalPlan (plan: StudyPlan): bool =
        plan |> Seq.forall (fun unitInPlan -> isLegalIn unitInPlan.code unitInPlan.semester plan) 


// Functions returning various information about units ...
// Returns all of the unit codes that are mentioned anywhere in the prerequisites of the specified unit
let UnitPrereqs (unitCode:UnitCode) : seq<UnitCode> = 

    /// flatten each and() and or() into a sequence of unit codes
    let rec unpackPrereqs (prerequisite:Prereq) : seq<UnitCode> =
        match prerequisite with  
        | Unit(code) -> seq { yield code }
        | And(andPrereqs) ->
            andPrereqs 
            |> Seq.map unpackPrereqs // get all unit codes within And()
            |> Seq.concat // flatten sequence
        | Or(orPrereqs) ->
            orPrereqs 
            |> Seq.map unpackPrereqs // get all unit codes within Or()
            |> Seq.concat 
        | _ -> Seq.empty // return nothing since no unit codes are mentioned

    unpackPrereqs (lookup unitCode).prereq


// The title of the specified unit
let getUnitTitle (unitCode:UnitCode) : string = 
    unitCode + " " + (lookup unitCode).title


// The prerequisites of the specified unit as a string
let getPrereq (unitCode:UnitCode) : string = 
    if (lookup unitCode).prereqString = "" then
        "Prereqs: Nil" 
    else
        "Prereqs: " + (lookup unitCode).prereqString

// The semesters that the specified unit is offered in as a string
let displayOffered (unitCode:UnitCode) : string =
    let semList = 
        (lookup unitCode).offered 
        |> Set.toList

    let semStr = 
        semList 
        |> List.map ( fun sem -> 
            match sem with
            | Semester1 when sem = semList.Head -> "semester 1" // if the first sem it is offered in is sem 1
            | Semester1 -> "1"
            | Semester2 when sem = semList.Head -> "semester 2" // if the first sem it is offered in is sem 2
            | Semester2 -> "2"
            | Summer -> "summer"
        )

    semStr |> String.concat " or " 


// The specified semester as a string (format: year/semester)
let display (sem:Semester) : string = 
    (string) sem.year + "/" +
    match sem.offering with
    | Semester1 -> "1"
    | Semester2 -> "2"
    | Summer -> "S"