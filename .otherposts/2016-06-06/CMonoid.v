
Require Import Coq.Lists.List.
Require Import Coq.Arith.Plus.

Open Scope list_scope.

Definition assoc {O : Type} (unit : O) (op : O -> O -> O) : Prop :=
    forall {x y z : O}, op x (op y z) = op (op x y) z.

Definition unit_cancel {O : Type} (unit : O) (op : O -> O -> O) : Prop :=
    forall {x : O}, op x unit = x /\ op unit x = x.

Definition monoid_laws {O : Type} (unit : O) (op : O -> O -> O) : Prop :=
    assoc unit op /\ unit_cancel unit op.

Inductive Monoid (O : Type) :=
    monoid
        (unit : O)
        (op : O -> O -> O)
        (proof : monoid_laws unit op)
            : Monoid O.

Definition unit_of {O : Type} (mon : Monoid O) :=
    match mon with
        monoid unit _ _ => unit
    end.

Definition op_of {O : Type} (mon : Monoid O) :=
    match mon with
        monoid _ op _ => op
    end.

Definition list_monoid (X : Type) : Monoid (list X).
    refine (monoid (list X) nil (@app X) _).
    split.
        unfold assoc.
        apply app_assoc.
        
        split.
            apply app_nil_r.
            apply app_nil_l.
Defined.

Definition commut {O : Type} (unit : O) (op : O -> O -> O) : Prop :=
    forall {x y : O}, op x y = op y x.

Definition cmonoid_laws {O : Type} (unit : O) (op : O -> O -> O) : Prop :=
    monoid_laws unit op /\ commut unit op.

Inductive CMonoid (O : Type) :=
    cmonoid
        (unit : O)
        (op : O -> O -> O)
        (proof : cmonoid_laws unit op)
            : CMonoid O.

Definition monoid_of {O : Type} (mon : CMonoid O) :=
    match mon with
        cmonoid unit op (conj laws _) => monoid O unit op laws
    end.

Definition cunit_of {O : Type} (mon : CMonoid O) :=
   unit_of (monoid_of mon).

Definition cop_of {O : Type} (mon : CMonoid O) :=
   op_of (monoid_of mon).

Fixpoint pow {O : Type} (mon : CMonoid O) (val : O) (n : nat) :=
    match n with
        | 0 => cunit_of mon
        | S n' => cop_of mon val (pow mon val n')
    end.

Definition Sub (O : Type) := O -> Prop.

Definition In {A : Type} (a : A) (b : Sub A) := b a.

Notation "a << b" := (In a b) (at level 50, no associativity).

Inductive sequence (O : Type) :=
    seq : {f : nat -> option O | forall n, f n <> None -> f (S n) <> None} -> sequence O.

Fixpoint span {O : Type} (mon : CMonoid O) (lis : list O) : Sub O :=
    fun val : O =>
        match lis with
            | nil => val = cunit_of mon
            | v :: rest =>
                span mon rest val \/ exists u n, cop_of mon (pow mon v n) u = val
        end.

Definition nat_plus : CMonoid nat.
    refine (cmonoid _ 0 plus _).
    unfold cmonoid_laws.
    unfold monoid_laws.
    unfold commut.
    unfold assoc.
    unfold unit_cancel.
    split.
        split.
            apply plus_assoc.
            split. apply plus_0_r. apply plus_0_l.
        apply plus_comm.
Defined.

Theorem one_spans : forall n, span nat_plus (1 :: nil) n.
    intros n.
    unfold span.
    Hint Unfold cunit_of unit_of monoid_of cop_of op_of.
    repeat autounfold.
    unfold nat_plus at 1 2.
    right.
    exists 0.
    exists n.
    rewrite plus_0_r.
    induction n.
        unfold pow; unfold nat_plus; repeat autounfold; reflexivity.
    simpl.
    repeat autounfold; unfold nat_plus.
    fold nat_plus.
    rewrite IHn.
    unfold plus.
    reflexivity.
Qed.
