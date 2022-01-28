# -*- coding: utf-8 -*-
"""
Created on Fri Jan 28 12:13:38 2022

@author: EYC
"""

import pyreadr
import numpy as np
import tensorflow as tf

from sklearn.metrics import r2_score
from sklearn.model_selection import train_test_split
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Dropout
from tensorflow.keras.optimizers import Adam

from tensorflow.keras.optimizers import schedules
from tensorflow.keras.optimizers import SGD
import platform
print(platform.python_version())

train_file = "/home/ychen42/GDSC1 Laptabnib-20220120T225959Z-001/GDSC1 Laptabnib/Lapatinib_trainFrame.RData"
trainFrame =pyreadr.read_r(train_file)['trainFrame']
trainFrame.shape

test_file = "/home/ychen42/GDSC1 Laptabnib-20220120T225959Z-001/GDSC1 Laptabnib/Lapatinib_testFrame.RData"
testFrame =pyreadr.read_r(test_file)['testFrame']
testFrame.shape

X_train = trainFrame.iloc[:,1:].values
X_test = testFrame.values
y = trainFrame.iloc[:,0].values


#################### relu neural network #########################
#Variables
layerOneNumNodes = 128
layerTwoNumNodes = 128
layerThreeNumNodes = 128

activation = 'relu'

learningRate = 0.01
learningRateDecay = 1

regressor = Sequential()
regressor.add(Dense(units = layerOneNumNodes, input_shape = (X_train.shape[1],), activation = activation))
regressor.add(Dropout(0.4))

regressor.add(Dense(units = layerTwoNumNodes, activation = activation))
regressor.add(Dropout(0.4))

regressor.add(Dense(units = layerThreeNumNodes, activation = activation))
regressor.add(Dropout(0.4))
regressor.add(Dense(units = 1,  activation = 'linear'))

lr_schedule = schedules.ExponentialDecay(
  initial_learning_rate=learningRate,
  decay_steps=10000,
  decay_rate=learningRateDecay)
sgd = tf.keras.optimizers.SGD(
  learning_rate=learningRate, momentum=0.0, nesterov=False, name="SGD"
)

opt = Adam(learning_rate=lr_schedule)


regressor.compile(optimizer = opt, loss = 'mse')

history = regressor.fit(X_train, y, epochs = 50, batch_size = 16, shuffle=True)

#save photo
predictions = regressor.predict(X_test)
predictions=np.reshape(predictions, (predictions.shape[0],))
print(predictions)

file = "/home/ychen42/GDSC1 Laptabnib-20220120T225959Z-001/GDSC1 Laptabnib/Lapatinib_DLpredict.csv"
np.savetxt(file, predictions, fmt="%s", delimiter=',')

#################### mlp network #########################
from sklearn.neural_network import MLPRegressor

mlp = MLPRegressor(max_iter=100, random_state=0)
mlp.fit(X_train, y)
predictions = mlp.predict(X_test)
file = "/home/ychen42/GDSC1 Laptabnib-20220120T225959Z-001/GDSC1 Laptabnib/Lapatinib_DLMPLpredict.csv"
np.savetxt(file, predictions, fmt="%s", delimiter=',')

