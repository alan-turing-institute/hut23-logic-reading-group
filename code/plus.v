Module NatExample.

  Inductive nat : Type :=
  | O
  | S (n : nat).

  Check S O.

  Fixpoint plus (n : nat) (m : nat) : nat :=
    match n with
    | O => m
    | S n' => S (plus n' m)
    end.

  Compute (plus (S (S O)) (S O)).

End NatExample.


Lemma left_id_O : forall m : nat, plus 0 m = m.
Proof.
  intros.
  reflexivity.
Qed.

Lemma right_id_O : forall m : nat, plus m 0 = m.
Proof.
  intros.
  induction m as [ | n' IHn'].
  - reflexivity.
  - simpl.
    rewrite -> IHn'.
    reflexivity.
Qed.

Lemma plus_n_Sm : forall n m : nat, n + S m = S (n + m).
Proof.
intros.
  
Theorem plus_commutativity : forall m n : nat, plus m n = plus n m.
Proof.
intros.
induction m as [ | m' IHm'].
-
  simpl.
  rewrite -> right_id_O.
  reflexivity.
-
  simpl.
