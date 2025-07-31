module MonsterAttack exposing (..)


type alias MonsterDamage =
    String


attackWithSword1 : MonsterDamage -> Int -> MonsterDamage
attackWithSword1 monsterDamage strength =
    monsterDamage
        ++ "Attacked with sword of strength "
        ++ (String.fromInt strength)
        ++ "."


attackWithClaw1 : MonsterDamage -> Int -> MonsterDamage
attackWithClaw1 monsterDamage strength =
    monsterDamage
        ++ "Attacked with claw of strength "
        ++ (String.fromInt strength)
        ++ "."


attackByA1: MonsterDamage -> MonsterDamage
attackByA1 m =
    attackWithSword1 m 5
attackByK1: MonsterDamage -> MonsterDamage
attackByK1 m =
    attackWithClaw1 m 1

attack1 : MonsterDamage -> MonsterDamage
attack1 =
    attackByA1 >> attackByK1 >> attackByK1 >> attackByA1


attackWithSword2 : Int -> MonsterDamage -> MonsterDamage
attackWithSword2 strength monsterDamage =
    attackWithSword1 monsterDamage strength


attackWithClaw2 : Int -> MonsterDamage -> MonsterDamage
attackWithClaw2 strength monsterDamage =
    attackWithClaw1 monsterDamage strength


attack2 : MonsterDamage -> MonsterDamage
attack2 monsterDamage =
    monsterDamage
        |> attackWithSword2 5
        |> attackWithClaw2 1
        |> attackWithClaw2 1
        |> attackWithSword2 5


attack3 : MonsterDamage -> MonsterDamage
attack3 =
    (attackWithSword2 5)
    >> (attackWithClaw2 1)
    >> (attackWithClaw2 1)
    >> (attackWithSword2 5)
