# §3 The feeling body — Carspecken's account of subjective validity's ground

*Target: 1,000–1,300 words. **Draft state:** written without the NotebookLM trawl the paper-skeleton designates as prerequisite. Passages marked `[TRAWL]` are placeholders where Carspecken-notebook material needs to be substituted in. Tio should treat this draft as scaffolding: the argument structure stands, the specific Carspecken citations need sourcing.*

---

Best Day worked in a medium that resists full explication. Students carried their experiences of difference into the classroom as felt things — things they had lived through and had some pre-reflective grasp of before language. The dyadic questions asked pairs of students to articulate the inferential relationships between their experiences. Articulation is the move from the felt into the sayable. Something precedes the saying, and something remains after the saying that the saying has not fully absorbed.

Phil Carspecken's horizon analysis calls the preceding thing the *feeling body* and locates it in the ground of subjective validity claims. I draw on his account to give PML's subjective-mode axioms a home in embodied experience rather than in introspection-as-observation. This section is the theoretical weight-bearing section of the paper. It is also the section most under-sourced in the current draft, because the genealogy of how this concept lives in my own notebook has not yet been trawled out.

## Three registers of validity

Carspecken distinguishes three registers in which a claim can be valid: *objective* (the claim is about states of affairs anyone could check), *subjective* (the claim is about the claimant's own inner state, accessible only to her from the inside), and *normative* (the claim is about how we ought to go on, about what the community should commit to). The registers are not three kinds of claim but three dimensions of the same claim. A speaker saying "the room is too warm for the students to concentrate" is making an objective claim about the room, a subjective claim about her own sense of the room's warmth, and a normative claim about what should be done given the warmth. Horizon analysis is the discipline of keeping the three registers distinguished while tracking how they shift under conversational pressure.

The subjective register is the hardest of the three to ground. Objective claims are answerable in principle to whatever counts as observation in the relevant domain. Normative claims are answerable to the community of practitioners whose norms are at stake. Subjective claims carry a structural asymmetry: the claimant has privileged access, and no observer can overrule the claimant's own report on her inner state without doing violence to the structure of subjective testimony.

The asymmetry is what makes subjective validity philosophically difficult. If I cannot be overruled on what I feel, the account risks collapsing into sheer assertion. If I can be overruled, the asymmetry fails and the register is not really subjective at all. Carspecken's move is to locate subjective validity in a ground that is neither pure assertion nor external observation: the feeling body.

## What the feeling body is not

Introspection-as-observation is the view that the mind is a kind of inner room one can turn one's attention toward and see what is inside. The claim "I feel anxious" would, on this account, report the result of looking inward and finding anxiety. Carspecken's objection, drawing on threads from phenomenology that the NotebookLM material will source precisely, is that this account turns the self into an object. If the self is an object, then the asymmetry between first-person and third-person access is not structurally distinctive. It becomes a contingent matter of epistemic privilege — I happen to be closer to my own mind than you are, the way I am closer to the inside of my own mouth.

The feeling body is supposed to be something different from an observable inner room. It is the pre-reflective embodied sense of one's own state — not a thing one observes, but a way of being that informs observation without being its object. `[TRAWL: Carspecken on the feeling body as ground, not as observed inner region. Expect passages on how the feeling body makes subjective validity claims possible without committing one to introspective observation.]`

`[TRAWL: expected material on how the feeling body relates to language and articulation. Carspecken's account should explain how feeling-body content is available for reflective articulation without being reducible to what articulation renders.]`

## Why PML has an attributed variable that refuses unification

In the PML Prolog implementation, the subjective mode wrapper `s(_)` accepts any proposition as a term. Underneath, the pragmatic axioms use a feature of Prolog's logic engine called *attributed variables*. The I-feeling — the computational analogue of the feeling-body's first-person self-sense — is implemented as a variable that carries an `arche_trace` attribute. The attribute has a hook that fires whenever the variable is unified with anything concrete. The hook fails the unification. The I-feeling can pass cleanly through structural reasoning (it can be carried along as a variable), but it cannot be identified with any concrete term.

The philosophical reason for the refusal is that identifying the I-feeling with a concrete term is precisely the move the account of the feeling body is trying to avoid. A concrete term is an object. Identifying the self-feeling with an object performs the picture-thinking the account warns against. The refusal of unification is a formal analog for a philosophical prohibition.

The consequence inside the proof system is modest but structurally honest. When a sequent contains a variable carrying the `arche_trace` attribute, the proof object that the sequent calculus constructs is not an ordinary `proof(...)` term but a hollow `erasure(...)` term. The derivation succeeds structurally — the rules all fire, the sequent closes — but the proof object explicitly marks that the content carried by the trace-bearing variable has not been pinned down. The formalism builds in a marker for *where it stops being able to say anything*.

This marker is not a bug. It is the mechanical analog of the point at which discourse itself hits the edge. When a student says "I don't know why 2+3=5, but I'm sure it does" or "Oh, well, you know what I mean," justification has run out. The teacher cannot extract a proof from the student that would ground the claim further. The student is standing on the feeling body, and the teacher is not welcome inside. Something is still being communicated — the student's commitment is visible even when the ground of the commitment is not — but the communication does not take the form of a fully explicit proof.

## Two questions the section opens

The first question is *what the dynamics internal to subjective validity claims look like*. Given that subjective claims rest on the feeling body, how do sub-claims within the subjective register relate to one another? How does "I feel anxious" interact with "I feel committed to these students" when the two coexist in the same teacher's report? §4 presents the PML axioms that answer one version of this question.

The second question is *how subjective moves transfer to objective and normative moves*. How does a student's felt sense of alienation in a gym become an objective description of that gym's spatial grammar, and under what conditions does that in turn become a normative claim about what we owe each other in spatial arrangements? PML's S→O transfer axioms ("the Oobleck dynamic") gesture at this; the N-mode internalization axioms ("N→S") are named but not yet coded. §4 reports on the gesture; §6 reports on the gap.

`[TRAWL: expected material on how Carspecken's horizon analysis handles inter-register transfer. In particular, passages on how pragmatic pressure shifts a claim from one register to another without the claimant intending the shift.]`

## Transition to §4

PML's subjective-mode axioms are a specific, falsifiable-in-principle, currently-unfalsified stake in the ground: here is a vocabulary for the dialectical movement of subjective claims under the Hegelian rhythm of unity, awareness, tension, letting-go, return-to-a-transformed-unity. The axioms are drawn from Hegel's *Science of Logic* and from Brandom's *A Spirit of Trust* reading of Hegel, and they are presented in Prolog as material inferences. The next section presents them in English.
