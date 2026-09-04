#import "@local/tempst:0.1.0": *
#show: exercise.with(
  title:         "Exercises 1",
  course:        "AI510 - Cybersecurity and Innovation",
  author:        "Simon Holm",
  date:          "Fall - 2026",
  outline:       true,
  outline-depth: 2,
)

= Exercise 1
#question(title: "1.1")[
Explain, in your own words, the three protection goals that make up the C-I-A triad. Which two protection goal were added later on?
]

#answer[
- Confidentiality (data should be unavailable to others)
- Integrity (guarding information)
- Availability (backups)
- Authenticity (ensure that those who are sharing data are who they say they are)
- Non-repudiation. (ensuring that both sender and receiver knows that data sharing has taken place.)
]

#question(title: "1.2")[
1.2: How was risk mathematically defined in the lecture?
]
#answer[
 $ R_a = p_a dot d_a $
    probability of attack $times$ damage
]

#question(title: "1.3")[
Which questions are indicative of a security mindset?
]
#answer[
- Which part of the system can the user control
- What can they do outside of what is intended from  the system
- Is any of the unintended functionality useful
]

#question(title: "1.4")[
What are the three steps when creating an attack tree?
]
#answer[
- Create the tree
- Prune the tree (just gray out things that are already invalid)
- Think about countermeasures for all leafs
]
#pagebreak()

#question(title: "1.5")[
What is the defining property of symmetric encryption opposed to asymmetric encryption?
]
#answer[
Symmetric encryptions shares 1 key which can be vulnerable, asymmetric encryptions uses both public and private key such that Alice can use Bobs public key to encrypt data that only Bob can decrypt. 
]

#question(title: "1.6")[
What are the three defining properties of cryptographic hash functions?
]
#answer[
- One way function (its infeasible to find $x st H(x)=h$ given an $h$) 
- Collision-resistance (No pair $(x,y)$ exists such that $H(x) = H(y)$)
- Resource efficient 
]

#question(title: "1.7")[
Briefly explain, in your own words, the basic idea behind hybrid encryption.
]
#answer[
Hybrid encryption, encrypts a symmetric session $k$, which can more efficiently decipher the message $m$. The session $k$ Is onetime use.
]

= Exercise 2
Assume your goal is to get hold of the written exam for a specific course. You know that the laptop of the respective lecturer is consistently unlocked when they go to the bathroom. However, their office is on a restricted corridor.
#question(title: "2.1")[
Draw an attack tree for this scenario. Consider all three steps of creating an attack tree separately and draw the tree in all three steps. You don’t need a comprehensive tree, but make sure to have at least a few full paths with countermeasures.
]
#answer[
  + Wait for the professor to go to the bathroom
    + Get through the door to the restricted corridor
      + steal a key to the corridor
        + steal a key from another employee
          - *countermeasure* track key usage with logs
      + forge a key to the corridor
        - *countermeasure* Keys should be difficult to forge
      + break the door to the corridor
        - *countermeasure* Alarm
      + #strike[teleport into the office]

]

#question(title: "2.2")[
How does the tree change, if the lecturer shares the office with another lecturer? Draw the tree again or add a respective sub-tree in this scenario.
]
#answer[
+ Wait for the professor to go to the bathroom
    + Get through the door to the restricted corridor
      + steal a key to the corridor
        + steal a key from another employee
          + run past the other professor
            - *countermeasure* expel student 
          - *countermeasure* track key usage with logs
      + forge a key to the corridor
        - *countermeasure* Keys should be difficult to forge
      + break the door to the corridor
        - *countermeasure* Alarm
      + #strike[teleport into the office]
+ Wait for both professors to go to the bathroom
  - *countermeasure* professors should not go to the bathroom at the same time.
+ Kill the other professor beforehand
  - *countermeasure* replace the dead professor.
]
  
= Exercise 3
#question(title: "3.1")[
Explain the term Layered Security in your own words.
]
#answer[
One can assume that given enough layers of security. A perpetrator will be caught.
]
#pagebreak()

= Exercise 4
Assume a scenario like the “red telephone”, where two parties need to repeatedly communicate confidential information.

#question(title: "4.1")[
Which type of encryption is suitable for this scenario? Explain your answer.
]
#answer[
Asymmetric encryption (maybe hybrid)
]

#question(title: "4.2")[
Assume the number of parties grows to five. Which type of encryption is suitable for this scenario? Explain your answer.
]
#answer[
Still asymmetric encryption (maybe hybrid)
]

= Exercise 5
#question(title: "5.1")[
What are the benefits of hybrid encryption?
]
#answer[
Mostly efficiency
]

#question(title: "5.2")[
Can you imagine a scenario where this is used today? Give an example and explain why it makes sense in this case.
]
#answer[
Almost everywhere, for example https
]


= Exercise 6
#question(title: "6.1")[
Encrypt the bitstring $m$ = 10110110 01100111 using with the key $k$ = 01010110 01011100 using the One-Time-Pad algorithm. Assume bit-wise encryption.
]
#answer[
$ 10110110 space 01100111 xor 01010110 space 01011100 = 11100000 space 00111011 $
]
