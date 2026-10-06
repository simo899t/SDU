#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 6: Segmentation & Video generation",
  course: "AI508 - Comutervision",
  date: "Fall - 2026"
)

// content start here
= Segmentation tasks
- Semantic (split up into classes (like chair, floor))
- Instance (each instance of an object is unique, but only look for object (not backgrounds))
- Panoptic (semantic but each instance of an object is unique)

= Mask[2]Former

= OneFormer
Universal Model architecture and data

= Segment Anything (SAM)
- Zero-shot generalization
- #link("https://segment-anything.metademolab.com/")[Segment Anything - MetaAI]
- #link("https://arxiv.org/abs/2304.02643")[paper article]

== SAM2

= Segment Everything Everywhere All at Once (SEEM)


= Depth Anything
#link("https://depth-anything.github.io/")[Depth Anything]

== Depth Anything 2
Trained on correctly.labelled synthetic data instead of real data.
#link("https://depth-anything-v2.github.io/")[Depth Anything v2]