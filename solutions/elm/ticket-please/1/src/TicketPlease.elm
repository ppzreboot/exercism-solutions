module TicketPlease exposing (..)

import TicketPleaseSupport exposing (Status(..), Ticket(..), User(..))


emptyComment : ( User, String ) -> Bool
emptyComment (user, comment) =
    comment == ""


numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket { createdBy, comments }) =
    comments
    |> List.filter (\(commentUser, commentStr) -> 
            let
                (user, _) = createdBy
                (User createdByName) = user
                (User commentName) = commentUser
            in
                createdByName == commentName
    )
    |> List.length


assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket { createdBy, assignedTo }) =
    case assignedTo of
        Just (User at) ->
            let
                (creator, _) = createdBy
                (User creatorName) = creator
            in not (at == creatorName)
        _ -> False


assignTicketTo : User -> Ticket -> Ticket
assignTicketTo u (Ticket t) =
    Ticket { t | assignedTo = Just u }
