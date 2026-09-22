#import "@local/tempst:0.1.0": *

#show: note.with(
  title: "Lecture 3: YOLO \n (you only look once)",
  course: "AI508 - Comutervision",
  date: "Fall - 2026"
)

// content start here
= Bounding box
- Corner based
upper-left  $(x_1,y_1)$ & lowe right corner $(x_2,y_2)$
- Center & size
Center point $(x,y)$ & with/height $(w,h)$
- Normalized center & size
Center point $(x\/W, y\/H)$ and width and height $(w\/W, h\/H)$
- Anchor

= Intersection over union (IoU)

#figure(
  image("figures/figure-2.svg"),
  caption: [For a set of bounding boxes like above. $ "IoU" = A(B_1 cap B_2)/A(B_1 cup B_2) $],
)
IoU of $0.5$ is considered a good overlap
#pagebreak()

= mean Average Precision
- Precision-Recall Curve (PRC):
As recall increases, precision drops
- Average Precision (AP):
Area under precision-recall curve (AUPRC) (class specific)
- mean Average Precision (mAP)
Scalar metric for model performance (across classes)
- IoU threshold often indicated
$ "mAP" dot "minIoU" $

= R-CNN (region cnn)
Starts off by selective search using oversegmentation, and joining of similar segments
$ s(r_1,r_2) & = alpha_1 dot s_"color" (r_1,r_2) + alpha_2 dot s_"texture" (r_1,r_2) \ & space + alpha_3 dot s_"size" (r_1,r_2) + alpha_4 dot s_"shape" (r_1,r_2) $ 
This should detect different concepts like
- Color similarity: color histogram intersection
- Texture similarity: texture histogram intersection
- Size similarity: 1-relative combined size
- Shape similarity: 1-bounding box overhead

Aim for high recall!

== Support vector machines
Object classification via svm's
#figure(
  image("assets/image-1.png"),
  caption: [],
)

== Non-maximum suppression
Models might predict multiple overlapping bounded boxes for the same object.
+ sort all bounding boxes by confidence
+ Take bounding box with the highest confidence score
+ remove the strongly-overlapping ones according to an IoU threshold, typically $"IoU" >= 0.3$
+ Repeat step 1

== Bounding box offset regression

#figure(
  image("assets/image-2.png"),
  caption: [],
)

= SSD single shot detection


#pagebreak()

= YOLO: You only look once
Predicts multiple bounding boxes and class probabilities in one shot. 

#figure(
  image("assets/image-3.png"),
  caption: [],
)

== Prediction
For each cell predict $p_1,dots,p_C,overbrace((c_1,x_1,y_1,w_1,h_1), "BB"_1), dots, overbrace((c_B,x_B,y_B,w_B,h_B), "BB"_B)$

- Each region can then have $B$ bounding boxes
- 

== Loss function
$ loss &= lam_"coord" sum_(i=1)^(s^2) sum_(j=1)^(B) bb(1)_(i j)^"obj" [(x_i - hat(x)_i)^2 + (y_i - hat(y)_i)^2] \
&= lam_"coord" sum_(i=1)^(s^2) sum_(j=1)^(B) bb(1)_(i j)^"obj" [(sqrt(w_i) - sqrt(hat(w)_i))^2 + (sqrt(h_i) - sqrt(hat(h_i)))^2] \
&=  sum_(i=1)^(s^2) sum_(j=1)^(B) bb(1)_(i j)^"obj" (C_i-hat(C)_i)^2 \
&= lam_"noobj" sum_(i=1)^(s^2) sum_(j=1)^(B) bb(1)_(i j)^"noobj" (C_i-hat(C)_i)^2 \
&= lam_"noobj" sum_(i=1)^(s^2) sum_(j=1)^(B) bb(1)_(i j)^"noobj" (C_i-hat(C)_i)^2 $


