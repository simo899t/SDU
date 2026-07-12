#import "@local/tempst:0.1.0": *
#show: note.with(
  title: "Lecture 7: ",
  course: "AI504 - Knowledge Represntation",
  date: "16/03/2026"
)
// content starts here


/*
office hours:
Thursday afternoon
Check itsLearning
*/

= Verbs

We live in a world with 2 types of creatures
1. moonacks
2. woodchucks

We know all moonacks are woodchucks, but nothing else

1. Do all who fear all moonacks fear all woodchucks?
2. Do all who fear all woodchucks frar all moonacks?
3. Do all who love all who fear all moonacks, love all who fear all woodchucks?
4. Do all who love all who fear all woodchucks, love all who fear all moonacks?

-- in this example 1 and 3 are unknown, while 2 and 4 must be true.

Verbs are labels as $ arrow.r.long^"fears" $

1. Let $m = {1}, w = {1,2}, "where" 3 arrow.r.long^"fears" m $

Then ${1} psubset emptyset quad absurd$

2. Has to be true
   - suppose i fear all woodchucks
      
      Let $m$ be an arbitrary moonack, $m$ must be a woodchuck
      
      Therefore I fear $m$
  - Therefore i fear #u("all") moonacks

3. Let $m = {1}, w = {1,2}, "where" 3 arrow.r.long^"fears" m "and" 4 arrow.r.long^"loves" 3 $
   
   Then ${1} psubset emptyset quad absurd$

4. Has to be true
   - suppose i love an arbitrary person who fears all woodchucks
      
      Let $m$ be an arbitrary moonack, $m$ must be a woodchuck
      
      this arbitrary person must fear $m$
      
      Since I love this person
  - I must love #u("all") who fears #u("all") moonacks

This is confusing 3 should be true, and false should be false