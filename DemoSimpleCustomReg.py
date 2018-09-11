import autograd.numpy as np
from autograd import grad

# code from https://github.com/HIPS/autograd/blob/master/docs/tutorial.md
# only 3 lines added to include custom functionality!

# Build a toy dataset.
inputs = np.array([[0.52, 1.12,  0.77],
                   [0.88, -1.08, 0.15],
                   [0.52, 0.06, -1.30],
                   [0.74, -2.49, 1.39]])

targets          = np.array([1, 1, 0, 1])
debiased_targets = np.array([0, 0, 0, 0])

def sigmoid(x):
    return 0.5 * (np.tanh(x / 2.) + 1)

def logistic_predictions(weights, inputs):
    # Outputs probability of a label being true according to logistic model.
    return sigmoid(np.dot(inputs, weights))

def training_loss(weights):
    # Training loss is the negative log-likelihood of the training labels.
    preds = logistic_predictions(weights, inputs)
    label_probabilities = preds * targets + (1 - preds) * (1 - targets)
    label_probabilities_debiased = preds * debiased_targets + (1 - preds) * (1 - debiased_targets)
    return 5 * -np.sum(np.log(label_probabilities)) + -np.sum(np.log(label_probabilities_debiased))


# Define a function that returns gradients of training loss using Autograd.
training_gradient_fun = grad(training_loss)

# Optimize weights using gradient descent.
weights = np.array([0.0, 0.0, 0.0])
print("Initial loss:", training_loss(weights))
for i in range(100):
    weights -= training_gradient_fun(weights) * 0.01

print("Trained loss:", training_loss(weights))
print(logistic_predictions(weights, inputs))
