module TicketPlease exposing (..)

import TicketPleaseSupport exposing (Status(..), Ticket(..), User(..))


emptyComment : ( User, String ) -> Bool
emptyComment (_, comment) =
    comment == ""


numberOfCreatorComments : Ticket -> Int
numberOfCreatorComments (Ticket { createdBy, comments }) =
    comments
    |> List.filter (\((User commentName), _) -> 
            let
                (user, _) = createdBy
                (User createdByName) = user
            in
                createdByName == commentName
    )
    |> List.length


assignedToDevTeam : Ticket -> Bool
assignedToDevTeam (Ticket { assignedTo }) =
    case assignedTo of
        Just (User "Alice") -> True
        Just (User "Bob") -> True
        Just (User "Charlie") -> True
            -- at == "Alice" || at == "Bob" || at == "Charlie"
        _ -> False

assignTicketTo : User -> Ticket -> Ticket
assignTicketTo u (Ticket ({ status } as t)) =
    case status of
        New -> Ticket { t | status = InProgress, assignedTo = Just u }
        Archived -> Ticket t
        _ -> Ticket { t | assignedTo = Just u }
