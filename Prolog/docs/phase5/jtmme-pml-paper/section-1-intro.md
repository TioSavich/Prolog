# §1 Introduction — a report, not a study

*Target: 500–700 words*

---

This paper is a report from inside a long-running formalization project. It is not an empirical study. It is not a framework paper announcing a new method. It is a report, submitted to a venue that welcomes marginal work, asking for reviewer feedback while the work is still moving.

The project runs on three intertwined strands. A classroom intervention called Best Day used AI-scaffolded dyadic questioning to make implicit connections between twenty-two preservice teachers' reflective papers available for discussion. A formalization called Polarized Modal Logic (PML) encodes a twelve-operator vocabulary for tracking the pragmatic stance of claims in the space of reasons. And a Prolog implementation of that vocabulary runs a small sequent calculus whose proof objects can go hollow at specific points — an honest formal marker for where justification runs out and human judgment must take over.

The paper presents the PML axioms for one of the three modes of validity — the subjective mode, which carries claims grounded in the feeling body — and reports what the code does and does not yet do. Best Day appears first, as the praxis that forced the formalization. The intervention worked; the question of why it worked was unanswerable without something like the vocabulary that PML now supplies. I do not claim the vocabulary is correct. I claim it is a specific enough stake in the ground that a reader can tell me where the stake is poorly placed.

Three commitments organize the paper.

The first is *a report's discipline about its own limits*. A report names what it has done, what it has not done, and what it cannot yet tell from the work done so far. This paper's §6 is a first-class section, not a buried paragraph of hedging. The code establishes internal consistency; it does not establish that the vocabulary tracks any observable phenomenon. The section on neurosymbolic testing is a proposal the author has not yet executed.

The second is *modality in the grammar of recognition*. The paper's theoretical backbone is Brandom's reading of Hegel in *A Spirit of Trust*: modality is not a layer added on top of cognition but a structural feature of the practice of giving and asking for reasons. PML's operators — compressive and expansive, necessity and possibility, subjective, objective, normative — are an attempt to make that grammar computationally inspectable. Whether the attempt succeeds is an open question.

The third is *the feeling body as phenomenological ground*. The paper draws on Carspecken's horizon analysis to locate subjective validity claims in pre-reflective embodied experience. The PML's subjective-mode axioms are read from Carspecken's account; the Prolog implementation carries a device (an attributed variable that refuses unification) whose philosophical motivation is precisely Carspecken's point that the self cannot be treated as an object available for inspection. Where the device succeeds, it marks the refusal. Where it fails, it fails in ways that the paper reports honestly.

A reader who knows Prolog will find the code fact-sheets referenced throughout at `docs/phase5/fact-sheet-<module>.md` in the project repository. A reader who does not should not need the Prolog to follow the paper. The §4 presentation of the S-mode axioms is in English. The dialectical movement is what the reader needs; the syntax is receipt, not subject.

The paper closes by naming five questions the author is asking the community for feedback on. It is not a conclusion. The work is still moving.

---

**Transition to §2:** The Best Day intervention ran once, in one class, with no systematic data collection. It was not a study. It was the praxis that made the formalization necessary, because the cognitive overload of connecting twenty-two students' reflective papers exceeded what I could hold in mind. The next section describes what happened.
