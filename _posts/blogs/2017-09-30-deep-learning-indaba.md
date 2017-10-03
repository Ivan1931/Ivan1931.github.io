---
title: Deep Learning Indaba - A retrospective
layout: default
permalink: /deep-learning-indaba/
categories: blogs
description: The deep learning indaba is a summit on deep learning and machine learning in Africa. The aim of the conference is to encourage the growth of African machine learning.

---

### About

This is a very rough retrospective on my experience at the 2017 Deep Learning Indaba and some of the new ideas I picked up while I was there. I'm by no means an ML expert but hopefully you might find some of the ideas here useful.

### On overfitting

Finally - after many years of misunderstanding - I finally think I have a good grasp on why we have a cross validation set for training
ML models. Some background context: when you're evaluating a machine learning algorithms predictive power on a data set you typically divide the data into 3 groups. They are `training set`, `test set` and `cross validation set`.  Each of which has the following functions:

* `training set` - train the model
* `test set` - tune the models hyper parameters to get better performance
* `cross validation set` - model performance validation

I used to be confused about the distinction between `test set` and `cross validation set`. The difference is very subtle and results from the problem of *overfitting*. If I'm tuning the hyper parameters of a model on some data - IE the test set - then you are trying to overfit a model to maximise performance. But what if I want to see whether the current model I'm iterating on is actually a good model? Is it a good idea to evaluate the performance of a model on data that I have actually tuned it on? Well no. The data you will encounter when you productionise the ML model will not necessarily be the data that you trained it on. So that's why a `cross validation set` exists. We 

### ML learns correlations - not causations

Recently, there was an article circulating around about how ML is being used to classify people as either "gay" or "not gay" based on 
their appearences. Nando de Freitas used an interesting example to illustrate why these types of experiements 
in classification are unethical and also not valuable. 
An example was given on how one could take a bunch of hipsters from boulder colorado and a bunch of prisoners in California and 
train a Neural Network to classify people as either "criminal" or "not criminal". This would probably work but only because the
network was learning a *correlation* based on existing biases in society. Correlation does not imply causation and taking action on the
predictions of such a model would be unethical for that reason. People are criminals because of things that they do - not due
to the result of some model.
Acting on such predictions only serves to reinforce existing bias in society. 

### Machine Learning is Perception

Some of the more interesting conversations I had at the Indaba were about the different fields of artificial intelligence and their implications. I'd like to think of machine learning as "perception" or *system 1* congition for those who have read *Thinking Fast and Slow*. An ML system is not learning how to reason in a high level deliberate fashion - it's learning what we consider instantaneous reflex reactions to stimuli. When I walked on to UCT campus the other day I thought of an example to illustrate the two modes of thinking. You could walk onto campus see a student and think "that student is definitely a zombie". This instantaneous judgment is *system 1* thinking. An example of *system 2* thinking is "final thesis was due this morning - obviously that student just pulled an all nighter and zombies don't exist." The scary HAL9000 advances in AI will not come from just *system 1* perception alone but from machines that can learn *system 2* style reasoning. It should be noted that there are many active areas of research on developing *system 2* style reasoning out there.

### If you have a gradient you should use it

This was a comment I overheard while going for lunch. The context of the comment was this - "When should I make use of evolutionary algorithms to solve my AI problem?" I had done an evolutionary computing course at UCT and started to see EC as a hammer and all problems mere nails. That being said - it's usually way faster and more predictable to model a problem as a function from which you can compute a gradient and then use gradient descent or one of the many other optimization methods. Evolutionary computing is for the times where you don't know what sort of function you're trying to optimize or you have a really complex environment to model. 

### Interactive Posters - do more of them

For my poster presentation at the indaba I made use of an interactive poster. It seemed to be quite well received even though it looked super last minute and shoddy. If there is one big takeaway from this blog post it's this - most people expect your poster presentation at a conference to be boring even if your work is awesome. Making an interactive poster will interest people and make you stand out. 

### Movies

Check out [Sunspring](https://www.youtube.com/watch?v=LY7x2Ihqjmc) . This is an amazing film generated by a LSTM neural network which stars Thomas Middlerich. It was recommended to me by a former tutor at Wits University. He found it quite hilarious while I found it really disturbing. I think the plot line contains a pretty disturbing chain of events including adultery, murder and what appears to be a black hole of doom. I recommend you watch it.  

### Misc

A great analogy is deep learning is like lego - you need a crap ton of small component parts to construct something truly impressive. 
