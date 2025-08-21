module GradeSchool exposing (Grade, Result(..), School, Student, addStudent, allStudents, emptySchool, studentsInGrade)

import Dict exposing (..)


type alias Grade =
    Int


type alias Student =
    String


type alias School =
    Dict Int (List Student)


type Result
    = Added
    | Duplicate


emptySchool : School
emptySchool =
    Dict.empty

alreadyExist : Student -> School -> Bool
alreadyExist stu school =
    allStudents school
    |> List.member stu

addStudent : Grade -> Student -> School -> ( Result, School )
addStudent grade student school =
    if alreadyExist student school then
        (Duplicate, school)
    else
        studentsInGrade grade school
        |> (::) student
        |> List.sort
        |> (\newStuList -> Dict.insert grade newStuList school)
        |> (\newSchool -> (Added, newSchool))
                


studentsInGrade : Grade -> School -> List Student
studentsInGrade grade school =
    Dict.get grade school
    |> Maybe.withDefault []


allStudents : School -> List Student
allStudents school =
    Dict.values school
    |> List.concat
