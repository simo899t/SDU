import numpy as np

def confusion_metrics_3x3(CM):
    """
    Input: CM - 3x3 confusion matrix (numpy array or list of lists)
    Output: prints accuracy, precision, recall, F1, and false positive rate per class
    """
    CM = np.array(CM)
    n = CM.shape[0]
    total = CM.sum()
    correct = np.trace(CM)

    accuracy = correct / total
    print(f"Overall Accuracy: {accuracy:.4f}\n")

    for i in range(n):
        TP = CM[i, i]
        FP = CM[:, i].sum() - TP
        FN = CM[i, :].sum() - TP
        TN = total - (TP + FP + FN)  # all other entries not in row/column i

        precision = TP / (TP + FP) if (TP + FP) != 0 else 0
        recall = TP / (TP + FN) if (TP + FN) != 0 else 0
        f1 = 2 * precision * recall / (precision + recall) if (precision + recall) != 0 else 0
        fpr = FP / (FP + TN) if (FP + TN) != 0 else 0

        print(f"Class {i+1}:")
        print(f"  Precision:  {precision:.4f}")
        print(f"  Recall:     {recall:.4f}")
        print(f"  F1-score:   {f1:.4f}")
        print(f"  FPR:        {fpr:.4f}\n")

# Example usage
CM = [[70, 20, 10],
      [10, 60, 10],
      [0, 10, 10]]

confusion_metrics_3x3(CM)
