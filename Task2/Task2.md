## Step 1c
    In summary, shallow trees can underfit/oversimplfy the data and can come to inacurate conlcusions that have high bias. Whereas deep trees can overfit the data (meaning it'll match not just the general trends of the data but the noise and inconsquential variations too). 
    It's best to find a balance between the two extremes where the tree can meaningfully follow the trends of the data but smooths over minor outliers in the data. 

## Step 1d
- Acuracy: overall correctnes of the model (times right -- pos and neg -- out of all guesses)
- Precision: proportion of true positives (times correct out of guessed positives)
- Recall: proportion of total correct positive (times correctly positive out of total positives) -- aka Sensativity

- If accuracy is high but recall is low, the model is likely missing many patients with heart disease, which is a critical issue for this task. This means the model is not sensitive enough to identify positive cases.
- If precision is high but recall is low, the model is very confident when it predicts heart disease, but it fails to identify all the patients who actually have heart disease. This could mean that the model is overly conservative, leading to missed diagnoses.
- If recall is high but precision is low, the model is catching most of the heart disease cases but may be over-predicting (leading to false alarms).

In this case, the Precision stays almost the same across all 4 depths (varrying only in the 3rd to 4th decimal place). Accuracy and Recall also stay pretty consistent. The lowest depth run is the only run with noticably lower scores. 

## Step 2c
- Accuracy (84.2%): The linear kernel has the highest accuracy. This suggests that the relationship between the features (like blood pressure, cholesterol, etc.) and the target (heart disease) is relatively linear in this dataset.
- Precision (89.1%): The Linear and default Sigmoid kernels have the highest precision, meaning that when they predicts heart disease, they're quite confident that the prediction is correct.
- Recall (74.6%): While recall is not the highest, the recall for the default Sigmoid and Linear kernels still perform better than other two kernels in balancing between capturing true positives (correctly predicting heart disease) and avoiding false positives.

## Step 2d
In the context of predicting heart disease, the trade-off between precision and recall is critical. A high precision model (like the linear and sigmoid kernels) minimizes false positives. A model with higher recall (sensitivity) would identify more actual heart disease patients, reducing false negatives (missed cases). While accuracy is an important metric, it can be misleading if the dataset is imbalanced (as it might be in the case of heart disease, where negative cases might be more common).

The Linear kernel offers a good balance between precision and recall, performing well overall for this dataset.
The Polynomial kernel underperforms, seemingly due to overfitting, and needs more work. 
The Sigmoid kernel performs similarly to the linear kernel, but tuning the coef0 parameter doesnt seem to  provide a ton of improvement (I did notice that when I made the coef0 slightly negative it seemed to perform better).

The models, in general, could benefit from further tuning or perhaps feature engineering to improve recall, ensuring that more heart disease cases are correctly identified.