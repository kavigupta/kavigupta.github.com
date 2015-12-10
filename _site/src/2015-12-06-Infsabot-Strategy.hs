type RobotProgramResult = (RobotAction, InternalState)
type RobotProgram = KnownState -> RobotProgramResult

data ActionType = TDie | TNoop | TFire | TDig | TMoveIn | TSpawn

data RP =
    DefaultRepresentation ActionType |
    ModifyMaterial ExprInt RP |
        -- Applies to Fire, Spawn
    ModifyDirection ExprDir RP |
        -- Applies to Move, Spawn, Fire, Send
    ModifyAppearance ExprInt RP |
        -- Applies to Spawn. RGB treated as a single integer.
    IfRP ExprBool RP RP

data ExprInt =
    ConstInt Int |
    RobAppear ExprPath ExprInt | -- Coordinate; Default if no robot
    MaterialLevel | -- Current material level
    Age | -- Current Age
    (:*) ExprInt ExprInt |
    (:+) ExprInt ExprInt |
    (:-) ExprInt ExprInt |
    (:/) ExprInt ExprInt |
    Mod ExprInt ExprInt |
    IfInt ExprBool ExprInt ExprInt |
    SwitchInt ExprDir ExprInt ExprInt ExprInt ExprInt

-- Basically [ExprDir]
    -- Written like this to make the tree more explicit
data ExprPath =
    Here |
    Offset ExprDir ExprPath

data ExprDir =
    ConstDir RDirection |
    IfDir ExprBool ExprDir ExprDir

data ExprBool =
    ConstBool Bool |
    CanSee ExprPath |
    MaterialAt ExprPath | -- False if can't see that far
    RobotAt ExprPath | -- False if can't see that far
    EqualInt ExprInt ExprInt |
    GreaterInt ExprInt ExprInt |
    EqualDir RDirection RDirection |
    Not ExprBool |
    (:&) ExprBool ExprBool |
    (:|) ExprBool ExprBool

class Evaluable given from to where
    eval :: given -> from -> to

instance Evaluable KnownState ExprInt Ratio where
    -- Evaluates to a Ratio, or fraction, for precision reasons.
    -- Round to get an actual integer
    ...
instance Evaluable KnownState ExprBool Bool where
    ...
instance Evaluable KnownState ExprDir RDirection where
    ...
instance Evaluable KnownState ExprPath [RDirection] where
    ...

digStrategy
    = IfRP (MaterialAt Here)
        (DefaultRepresentation TDig)
        (ModifyDirection
            (IfDir (MaterialAt (ConstDir N))
                N
                (IfDir (MaterialAt (ConstDir E))
                    E
                    IfDir (MaterialAt (ConstDir S))
                        S
                        W))
            (DefaultRepresentation TMove))

