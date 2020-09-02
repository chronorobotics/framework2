Framework for Testing Prediction Methods
========================================

Build & Run
-----------

1. Install the requirements by `cabal install Statistics hmatrix hmatrix-gsl hmatrix-special`
2. Build the experiment you are interested in, for example `ghc DoorState.hs`
3. Run the created executable, for example `./DoorState`

How to implement a prediction method
------------------------------------

In `Predictor.hs`, look at the definition of a method. A method is basically a function which is given a dataset as input and returns a predictor. You are free to implement this function as you wish. The output of each method is a predictor which assigns a predicted value for any time (and position).

**Do:**
* Implement the method as general as it makes sense. Making it more specific does not cost anything. Making it more general does.
* If it is appropriate, use `Data.Sequence` or `Data.Vector` instead of lists to save time.
* If a part of the method can be used in a different method also (with a few differences), implement it separately and maybe even more general than you need at the moment.

**Do NOT:**
* Do not hardcode any kind of magic numbers. Make them as parameters of the method instead. This applies even though you are not going to use any other value here. Hardcoded constants are allowed only when using a different value does not make sense for the given method (and not only the experiment you are now interested in).

How to implement an experiment
------------------------------

To create an experiment, you need a data loader (defined in `Scenario.hs`) and an error evaluator (defined in `Predictor.hs`). A data loader basically loads a dataset from given file. And an error evaluator is a function which calculates the error of a method on given dataset. To create a scenario (defined in `Scenario.hs`), you need both these and a name of the experiment.

After you create a scenario, you can process it by calling the function `processScenario`. The arguments of this functions are a scenario, a path to the training dataset, a path to the testing datasets and a list of used methods. When a method has parameters, you can either assign them here or use the function `bruteForceTrain` (defined in `Predictor.hs`) which tries all parameters from given list and chooses the best one.

The type of methods might not match the scenario. In this case, use method transformations to transform the methods into the the correct type. A few transformations are listed here:
* `methodChangeType` converts the input and output types.
* `methodFilterDataset` applies a filter to the dataset before the method is applied.
* `cellGridMethod` splits the input space into cells and the given method is applied to each cell independently.
* Is the transofmation available for `Method` but not for `MethodWithParams`? Apply `parametriseMethodTransform` to the transformation and it will be available for `MethodWithParams`.

**Do:**
* If the structure of the data loader or error evaluator can be used for other experiments with a few changes, consider making it more general.
* Use method transformations as long as it is possible.

**Do NOT:**
* Do not reinvent the wheel.
* Do not implement a method again with a few changes. Use method transformations instead.

TODO
----

* A way to save predictor for later use
* Graphical output
* Implement more methods
* Before we start implementing methods such as HyT-EM and HyT-CEM, implement probability distributions and EM-algorithm.
