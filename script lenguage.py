# -*- coding: utf-8 -*-
"""
Created on Thu Oct  5 09:57:49 2023

@author: ecarreno
"""

import os
import pandas as pd
from nltk.tokenize import word_tokenize, sent_tokenize
from nltk.stem import PorterStemmer
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import accuracy_score, precision_score
from sklearn.feature_extraction.text import CountVectorizer, TfidfVectorizer
from sklearn.naive_bayes import MultinomialNB


# dat = pd.read_csv("Language Detection.csv")
# dat = dat[(dat["Language"] == "English") | (dat["Language"] == "Spanish")]  
# 
# dat.to_csv("englishspanishtext.csv", sep = ";", index = False)

# dat = pd.read_csv("englishspanishtext.csv", sep = ";")

dat = pd.read_csv("data raw/englishspanishtext.csv", sep = ";")

# LIMPIEZA DE TEXTOS
import string
punc = string.punctuation

translator = str.maketrans('', '', punc)

def preprocess_text(text):
    # convert in lower case
    lower_text = text.lower()
    
    # word tokenization
    tokens = word_tokenize(lower_text)
    
    
    # remove special charactors and punctuations
    tokens2 = [token.translate(translator) for token in tokens if token not in punc]
    
    # stemming
    stm = PorterStemmer()
    stemmed_tokens = [stm.stem(token) for token in tokens2]
    
    preprocessed_text = ' '.join(stemmed_tokens)
    return  preprocessed_text


dat2 = dat[['Text', 'Language']]
dat2['Preprocessed_text'] = dat2['Text'].apply(preprocess_text)

X = dat2['Preprocessed_text']
y = dat2['Language']

# SPLIT Y ENTRENAMIENTO DEL MODELO
X_train, X_test,y_train, y_test = train_test_split(X,y,test_size=0.3)

label_encoder = LabelEncoder()
y_train_encoded = label_encoder.fit_transform(y_train)
y_test_encoded = label_encoder.transform(y_test)

cv = CountVectorizer()
X_train_cv = cv.fit_transform(X_train).toarray()
X_test_cv = cv.transform(X_test).toarray()

model = MultinomialNB()
model.fit(X_train_cv, y_train_encoded)

y_hat = model.predict(X_test_cv)

sum(y_hat == y_test_encoded)/len(y_test_encoded)


# IMPUTANDO IDIOMA EN DATOS PRE PROCESADOS
pre = pd.read_csv("data procesada/prepython_aniddata.csv", encoding = "latin1", sep = ";")

x_proyectos = pre["NOMBRE_PROYECTO"].apply(preprocess_text)

x_proyectos_encoded = cv.transform(x_proyectos)

y_language = model.predict(x_proyectos_encoded)

labels = label_encoder.classes_

pre["LENGUAJE"] = labels[y_language]

pre.to_csv("data procesada/postpython_aniddata.csv", 
            sep=";", encoding = "latin1", index=False)
















