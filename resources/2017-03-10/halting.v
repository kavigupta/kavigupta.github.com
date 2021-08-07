
Require Import Program.

Inductive exp :=
    | literal (value : bool)
    | inv_arg_self
    | inv_prog_arg (p : exp)
    | inv_arg_prog (p : exp)
    | inv (p q : exp)
    | if' (condition consequent alternative : exp).

Inductive result :=
    | terminated (output : bool)
    | cancelled.

Definition turing_value :=
    nat -> result.

Definition progUnpack (context : exp) (val : option exp) :=
    match val with
        | Some body => body
        | None => context
    end.

Inductive interpretsToInContext (arg : exp) (result : bool) : exp -> Prop :=
    | Literal
            : interpretsToInContext arg result (literal result)
    | InvocationArgSelf
        (proof : interpretsToInContext arg result arg)
            : interpretsToInContext arg result inv_arg_self
    | InvocationProgArg (f : exp) (proof : interpretsToInContext arg result f)
            : interpretsToInContext arg result (inv_prog_arg f)
    | InvocationArgProg (x : exp) (proof : interpretsToInContext x result arg)
            : interpretsToInContext arg result (inv_arg_prog x)
    | Invocation (f x : exp) (proof : interpretsToInContext x result f)
            : interpretsToInContext arg result (inv f x)
    | IfSo (co so al : exp) (proof : interpretsToInContext arg true co /\ interpretsToInContext arg result so)
            : interpretsToInContext arg result (if' co so al)
    | IfElse (co so al : exp) (proof : interpretsToInContext arg false co /\ interpretsToInContext arg result al)
            : interpretsToInContext arg result (if' co so al).

Definition interpretsTo : bool -> exp -> Prop :=
    interpretsToInContext (literal false).

Notation "a / b" := (interpretsTo b a).
Notation "a # b !! c" := (interpretsToInContext c b a) (at level 10).

Definition halts (ast : exp) := ast / true \/ ast / false.
Definition runsForever (ast : exp) := ~ ast / true /\ ~ ast / false.

Definition runsForeverOnAllInputs (ast : exp) :=
    forall arg, ~ (ast # true !! arg) /\ ~ (ast # false !! arg).

Definition forever := inv inv_arg_self inv_arg_self.

Theorem foreverIsForever : runsForeverOnAllInputs forever.
    split;
    intros H;
    dependent destruction H;
    dependent induction H;
    intuition.
Qed.

Definition turing (halt : exp) :=
    if' (inv_prog_arg halt) forever (literal false).

Theorem turingForeverOnInputHalts (halt : exp)
    : forall P, inv halt P / true -> runsForever (inv (turing halt) P).
    intros P halts; split; intros contr;
    dependent destruction contr;
    dependent destruction contr;
    try (destruct proof as [_ proof];
    fold forever in proof;
    destruct (foreverIsForever P); intuition;
    dependent destruction proof; fail).
    destruct proof as [proof _].
    dependent destruction proof.
    dependent destruction halts.
    unfold interpretsTo in halts.
    dependent destruction proof.
    dependent induction halts.
Qed.

Definition isHalting (H : exp) :=
    forall P, halts H /\ (halts P <-> inv H P / true).

Axiom classical : forall P, P \/ ~ P.

Theorem noHaltingProgram (H : exp) : ~ isHalting H.
    intros h.
    pose (t := inv (turing H) (turing H)).
    destruct (h t) as [Hh [f b]].
    destruct (classical (halts t)) as [Ht | Ht].
    pose (f' := f Ht).
    dependent destruction Ht.
    dependent destruction i.
    dependent destruction i.
Qed.
