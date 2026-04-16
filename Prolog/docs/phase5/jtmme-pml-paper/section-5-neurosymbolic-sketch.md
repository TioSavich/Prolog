# §5 What a test could look like, given recent neurosymbolic work

*Target: 800–1,000 words. The section sketches, with honest ownership of the fact that the author has not run the test.*

---

The axioms of §4 are internally consistent and empirically ungrounded. A plausible test would be whether the operator profile PML assigns to a classroom utterance predicts downstream discourse moves better than the same inputs routed through a baseline without the modal vocabulary. The recent neurosymbolic work that makes such a test technically feasible comes from two overlapping traditions: large-language-model perception of classroom talk (the TalkMoves corpus and the work coming out of the Sumner lab at Boulder), and the *logic-augmented language model* line whose most visible recent statement is Gary Marcus's continuing argument that symbolic structure must carry the inferential load that language models cannot reliably carry alone.

The Sumner lab's TalkMoves corpus is the accessible data surface. Suresh et al. (2018, 2019, 2021) and Jacobs et al. (2022) have labeled classroom utterances with seven or so discourse-move categories that index the pragmatic work an utterance does (*learning community*, *content knowledge*, *rigorous thinking*, plus sub-moves). Scornavacco and colleagues have worked the coding up to the level of aggregate teacher patterns. The labels are already in place; a researcher arriving new to the corpus does not need to hand-label utterances, and the inter-rater work has already been done. The question for a PML-based test is whether the PML operator profile carries *different* information than the TalkMoves labels carry, and whether that different information is load-bearing for some prediction.

## The proposed architecture

An LLM is prompted with a classroom utterance and a fixed menu of the twelve PML operators. The LLM returns a profile — a list of which operators the utterance is likely to be instantiating, with its own confidence per operator. The Prolog engine takes the profile as input and runs forward through the material inferences, producing a prediction about what the speaker is now licensed to claim (and what a hearer is licensed to expect the speaker to go on to claim). The prediction is tested against whatever label or downstream utterance the corpus already carries.

The minimal discipline the architecture imposes is that *LLM perception and Prolog inference are separable*. A second party should be able to take the same operator profile (same LLM output), route it through the same Prolog, and reproduce the prediction. The LLM's role is bounded: it assigns operator profiles; it does not do the modal reasoning. The Prolog's role is also bounded: it consumes operator profiles; it does not interpret utterances. The separation matters because it is the only way to get a reviewer's trust. If the LLM and the Prolog were coupled end-to-end, a failure could live in either and the architecture would not be inspectable.

A second piece of discipline is that *the full twelve-operator menu is given, not a narrowed subset*. Operators that never fire in a given discourse genre are themselves data. A corpus on primary-grade number talks may show `s◇↑` (subjective expansive possibility) dominating and `n□↓` (normative compressive necessity) absent. The absence is meaningful — it tells the analyst something about the register the teacher has invited students into. Narrowing the menu in advance would suppress that signal.

A third piece of discipline is *a baseline to beat*. The comparison is against an LLM doing the prediction task without PML — same corpus, same prompts, same downstream task, no modal vocabulary routed through Prolog. If the PML architecture does not beat the LLM-alone baseline, the modal vocabulary is not pulling its weight, and the paper's axioms are decorative rather than predictive.

A fourth is *preregistration of the prediction task*. What counts as a successful prediction has to be declared before any corpus opens. Otherwise the architecture will find a way to be right on the data it has already been shown, the way any framework elaborate enough to illuminate anything illuminates whatever happens to be in front of it.

## The Brandomian frame for why the test could work

Brandom treats deontic-normative vocabulary as a *pragmatic metavocabulary* for alethic modal vocabulary. What this means operationally: the normative moves a community makes in its discourse (who is taken to be committed to what, who is taken to be entitled to what, what counts as a reason) are what the alethic modal vocabulary (necessity and possibility) is *making explicit*. The N-mode operators in PML, in other words, are not redundant with the S and O modes. They are the operators that say *aloud* what the subjective and objective operators are implicitly doing.

A test that drew on this structure would not look for flat transition prediction from one utterance to the next. It would look for whether PML's N-mode operators, when the LLM assigns them to a run of utterances, make explicit a pattern of reasoning that the discourse as a whole is enacting. The form of the prediction is less "utterance A implies utterance B" and more "the operator pattern in this exchange is the alethic-modal shape of *this kind of classroom reasoning*, and when we see that shape we should also see these downstream markers." This is the shape of the test the paper would propose to the reviewer.

## What the author is asking for

The paper has not run this test. The author is asking the reviewer — particularly any reviewer with experience in the neurosymbolic tradition or in corpus-based classroom discourse work — two things. First, does the separation discipline (LLM profiles → Prolog inference → prediction, with baseline and preregistration) sound right for this kind of framework? Second, is there a reason the author has not foreseen that the test would fail on classroom corpora specifically? Classroom talk has structural features that may not align with what the PML axioms claim about dialectical movement. The author's guess is that the failure modes would be diagnostic: if PML predicts worse than the baseline in predictable regions (on teacher moves that are ritual-performative rather than recognitive, for instance), that is a finding about what the axioms are tracking and what they are not. The author would rather know the failure shape than not.

## Transition to §6

The test sketch is a proposal, not a result. The author's posture on the proposal is identical to the author's posture on the axioms: here is a specific-enough stake in the ground that a reader can tell the author where the stake is poorly placed. The next section is the author's own best account of where the stakes in this paper may be poorly placed.
