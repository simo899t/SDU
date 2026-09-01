#import "@local/tempst:0.1.0": *
#show: note.with(
  title:         "Lecture 1: Cybersecurity Fundamentals",
  course:        "AI510 - Cybersecurity and Innovation",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 3,
)

= What is Cybersecurity?

- *Security*
  - The process of protecting information by preventing, detecting, and responding to attacks.
  - Protection from attacks. (like a hacker)
- Safety
  - Freedom from conditions that can cause death, injury, occupational illness, damage to or loss of equipment or property, or damage to the environment.
  - Protection from unintended errors. (like an undetected bug)

== CIA + AN - by NIST
- *Confidentiality:* (encryption)
  - Ensuring that data or information is not made available or disclosed to unauthorized persons or processes.
- *Integrity:* (Cryptographic hashes)
  - Guarding against improper information modification or destruction in an unauthorized and undetected manner.
- *Availability:* (Backups)
  - Ensuring timely and reliable access to and use of information.
- *Authenticity:* (Message Authentication Codes)
  - Verifying that a user, process, or device is the one claimed, often as a prerequisite to allowing access to resources in an information system.
- *Non-repudiation:* (Digital Signatures with Received Receipt 
  - Assurance the sender of data is provided with proof of delivery and the recipient is provided with proof of the sender’s identity, so neither can later deny having processed the data.

_Note that depending on context there are even more_

_Also note that some cybersecurity protection goals like the ones above might contradict each other, like encryption and availability_

== Assumptions
One needs assumptions in order to create security, since no system that is sufficiently complex to be practically useful is 100% secure.
- There are always bugs in the code
- Side-channels to get secret info

Attackers strike wherever insufficient protection goals or unmet
assumptions allow it. They use these weak spots to build a so-called *Cyber Kill Chain*

== Cyber Kill Chain
#figure(
  image("assets/image.png"),
  caption: [By Lockheed Martin, #link("https://www.lockheedmartin.com/en-us/capabilities/cyber/cyber-kill-chain.html")],
) <fig:cyber-kill-chain>

A successfully exploited kill chain, can cause a great
variety of damages. We can consider relevant protection goals with *Risk and Threat Analysis*
#pagebreak()

== Risk and Threat Analysis
Risk matrices provide an abstract mathematical model
#definition(title: "Definition: Risk Matrix")[
  Risk $R_a$ of an attack $a$ is the product
  $ R_a = p_a dot d_a $

  where $p_a$ is the probability of attack $a$ succeeding and $d_a$ is the damage incurred by that attack if successful.
]

But what about the probability?

=== What is Risk
Risk is defined by matrices use very coarse probabilities and can serve as a start
#figure(
  image("assets/image-1.png"),
  caption: [By NASA, Goddard Technical Standard GSFC-STD-0002 – #link("http://everyspec.com/NASA/NASA-GSFC/GSFC-STD/download.php?spec=GSFC-STD-0002.020175.PDF")],
) <label>

It has been suggested that this might not be reliable enough in
cybersecurity (& elsewhere). So please use as exact probability estimates as possible, based on actual data even if it is sparse

=== What are Threats
- *Asset:* Any "thing" that holds value, cf. definition on slide 11 for examples.
- *Adversary:* Any entities (individual, group, organization, or government) that conducts or has the intent to conduct malicious activities.
- *Vulnerability:* A weakness in an information system, system security procedures, internal controls, or implementation that could be exploited or triggered by an adversary.
- *Threat:* Any circumstance or event with the potential to adversely impact an asset via exploiting a vulnerability.
- *Attack:* Any kind of malicious activity that realises a threat targetting an asset.
- *Countermeasure:* Any device or techniques that has as its objective the impairment of the operational effectiveness of attacks

#figure(
  image("assets/image-2.png"),
  caption: [Putting it all together],
) <label>


== Security Mindset
Any security "expert", needs a specific mindset
- Which parts of this do users have control over? Which ones shouldn’t they?
- What can this do outside of what it is meant to do?
- Is any of that unintended functionality useful?
#figure(
  image("assets/image-3.png"),
  caption: [Caption text],
) <label>

== Economic decision making
A countermeasure makes sense as long as the risk it mitigates is greater than its costs

$ L_(c a) = R_(a c) + C_c - (R_(a not c)) $

- $L_(c a)$: Losses incurred (damages & costs of countermeasures)
- $(R_(a not c))$: Risk in case attack $a$ without countermeasure $c$
- $R_(a c)$: Risk in case attack $a$ with countermeasure $c$
- $C_c$: Cost of countermeasure $c$

== Attack Trees
Attack trees allow mapping out risks systematically. This can create intuitive graphical overviews, when properly documented
#figure(
  image("assets/image-4.png"),
  caption: [Example on how to create an attack tree],
) <label>

Construct frameworks of attack terminology, e.g.
- Kill chain
- STRIDE
- MITRE ATT&CK

_Consider even unlikely attacks at this step_

=== STRIDE
Spoofing: Pretending to be someone/something you are not
- Tampering: Modify something you are not meant to modify
- Repudiation: Claim you didn’t do sth. (regardless of whether you did it)
- Information Disclosure: Information is exposed to unauthorized people
- Denial of Service: Attacks designed to prevent providing a service, e.g., by making it slow
- Elevation of Privilege: A user is able to do things in a system they are
not supposed to do

#figure(
  image("assets/image-6.png"),
  caption: [STRIDE - Examples],
) <label>


=== MITRE ATT&CK
- Adversarial Tactics, Techniques, and Common Knowledge
- Framework to document attack behaviour
- Establishes common taxonomy for attacks
- Models adversary behaviours
- Is in essence a knowledge base of adversarial techniques
- Techniques are sorted into categories
- Techniques can have sub-techniques
Freely available online: #link("https://attack.mitre.org/tactics/TA0043/")