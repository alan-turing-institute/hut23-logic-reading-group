From Stdlib Require Import Classical.


(* Tactics

exact H. Goal is H
exfalso.
contraction.
reflexivity.
replace.



 *)

Section Examples.

Variables P Q R : Prop.
  
  Lemma pq_p_and_q : P -> Q -> P /\ Q.
  Proof.
    intros p q.
    split.
    - exact p.
    - exact q.
  Qed.

  Print pq_p_and_q.

  Lemma p_and_q_pq : P /\ Q -> P.
  Proof.
    intro PQ.
    destruct PQ as [p q].
    assumption.
  Qed.

  Lemma p_p_or_q : P -> P \/ Q.
  Proof.
    intro p.
    left.
    assumption.
  Qed.

  Lemma or_elim : (P -> R) -> (Q -> R) -> (P \/ Q) -> R.
    intros pr qr p_or_q.
    destruct p_or_q as [p | q].
    apply pr in p.
    assumption.
    apply qr in q.
    assumption.
  Qed.

Print or_elim.  
