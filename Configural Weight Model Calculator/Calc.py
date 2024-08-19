import numpy as np 
import pandas as pd 
import random


def lotA_input():
    """
    docstring lottery A input function
    """
    lst_a_return = []
    lst_a_prob = []

    n_a = int(input("Enter number of branches of Lottery A : "))
    for i in range(0,n_a):
        A_return = float(input("Enter lottery outcome one at a time: "))
        while A_return<0:
                A_return = float(input("Please enter only positive returns: "))
        lst_a_return.append(A_return)
    for i in range(0,n_a):
        A_prob = float(input("Enter corresponding probabilities one at a time: "))
        while A_prob>1 or A_prob<0:
                A_prob = float(input("Please enter correct probability: "))
        lst_a_prob.append(A_prob)
    while sum(lst_a_prob)>1 or sum(lst_a_prob)<1:
        print("You entered wrong probabilities, please re-enter probabilities")
        lst_a_prob.clear()
        for i in range(0,n_a):
            A_prob = float(input("Enter corresponding probabilities: "))
            while A_prob>1 or A_prob<0:
                A_prob = float(input("Please enter correct probability: "))
            lst_a_prob.append(A_prob)
    ex_payoff_a = sum(np.multiply(lst_a_prob,lst_a_return))
    print("Lottery A")
    print("Outcomes are - " ,lst_a_return)
    print("Probabilities are - ", lst_a_prob)
    print("Expected payoff is:", ex_payoff_a)
    return

def lotB_input():
    """
    docstring lottery Binput function
    """
    lst_b_return = []
    lst_b_prob = []
    
    n_b = int(input("Enter number of branches of Lottery B: "))
    for i in range(0,n_b):
        B_return = float(input("Enter lottery outcome: "))
        while B_return<0:
                B_return = float(input("Please enter only positive returns: "))
        lst_b_return.append(B_return)
    for i in range(0,n_b):
        B_prob = float(input("Enter corresponding probabilities: "))
        while B_prob>1 or B_prob<0:
                B_prob = float(input("Please enter correct probability: "))
        lst_b_prob.append(B_prob)
    while sum(lst_b_prob)>1 or sum(lst_b_prob)<1:
        print("You entered wrong probabilities, please re-enter probabilities")
        lst_b_prob.clear()
        for i in range(0,n_b):
            B_prob = float(input("Enter corresponding probabilities: "))
            while B_prob>1 or B_prob<0:
                B_prob = float(input("Please enter correct probability: "))
            lst_b_prob.append(B_prob)
    ex_payoff_b = sum(np.multiply(lst_b_prob,lst_b_return))
    print("Lottery B")
    print("Outcomes are - " ,lst_b_return)
    print("Probabilities are - ", lst_b_prob)
    print("Expected payoff is:", ex_payoff_b)
    return

def utility_maximiser():
    """ comparison of two lotteries """
    lst_a_return = []
    lst_a_prob = []
    lst_b_return = []
    lst_b_prob = []
    sumw_a = 0
    sumw_b = 0
    sumx_a = 0
    sumx_b = 0

    n_a = int(input("How many branches does Lottery A have? : "))
    for i in range(0,n_a):
        A_return = float(input("Enter lottery returns once at a time: "))
        while A_return<0:
                A_return = float(input("Please enter only positive returns: "))
        lst_a_return.append(A_return)
    for i in range(0,n_a):
        A_prob = float(input("Enter corresponding probabilities: "))
        while A_prob>1 or A_prob<0:
                A_prob = float(input("Please enter correct probability: "))
        lst_a_prob.append(A_prob)
    while sum(lst_a_prob)>1 or sum(lst_a_prob)<1:
        print("You entered wrong probabilities, please re-enter probabilities")
        lst_a_prob.clear()
        for i in range(0,n_a):
            A_prob = float(input("Enter corresponding probabilities: "))
            while A_prob>1 or A_prob<0:
                A_prob = float(input("Please enter correct probability: "))
            lst_a_prob.append(A_prob)
    ex_payoff_a = sum(np.multiply(lst_a_prob,lst_a_return))
    n_b = int(input("Enter number of branches of Lottery B: "))
    for i in range(0,n_b):
        B_return = float(input("Enter lottery outcome: "))
        while B_return<0:
                B_return = float(input("Please enter only positive returns: "))
        lst_b_return.append(B_return)
    for i in range(0,n_b):
        B_prob = float(input("Enter corresponding probabilities: "))
        while B_prob>1 or B_prob<0:
                B_prob = float(input("Please enter correct probability: "))
        lst_b_prob.append(B_prob)
    while sum(lst_b_prob)>1 or sum(lst_b_prob)<1:
        print("You entered wrong probabilities, please re-enter probabilities")
        lst_b_prob.clear()
        for i in range(0,n_b):
            B_prob = float(input("Enter corresponding probabilities: "))
            while B_prob>1 or B_prob<0:
                B_prob = float(input("Please enter correct probability: "))
            lst_b_prob.append(B_prob)
    ex_payoff_b= sum(np.multiply(lst_b_prob,lst_b_return))
    theta = float(input("Please enter branch attention parameter here. Positive branch attention usually implies risk-averse, negative is risk-taker: "))
    beta = float(input("Please enter shape of utility function. For linear utility enter 1, for concave enter <1, for convex >1:  "))
    ex_payoff_b = sum(np.multiply(lst_b_prob,lst_b_return))
    gamma = float(input("Please enter positive gamma variable: "))
    #TAX THEORY CALCULATION
    #Lottery A
    for i in range(0, n_a):
        p = lst_a_prob[i]
        wa = p**gamma
        sumw_a = sumw_a +wa 
        sumx_a = sumx_a +wa* (lst_a_return[i]**beta)
        for j in range(i, n_a):
            pj = lst_a_prob[j]
            if theta>0:
                omeg = (theta/(n_a+1))*(p**gamma)
            else:
                omeg = (theta/(n_a+1))*(pj**gamma)
            sumx_a = sumx_a +omeg*((lst_a_return[j]**beta)-(lst_a_return[i]**beta))

    sumx_a = sumx_a/sumw_a
    tax_pred_a = sumx_a**(1/beta)
    #Lottery B
    for i in range(0, n_b):
        p = lst_b_prob[i]
        wa = p**gamma
        sumw_b = sumw_b +wa 
        sumx_b = sumx_b +wa* (lst_b_return[i]**beta)
        for j in range(i, n_b):
            pj = lst_b_prob[j]
            if theta>0:
                omeg = (theta/(n_b+1))*(p**gamma)
            else:
                omeg = (theta/(n_b+1))*(pj**gamma)
            sumx_b = sumx_b +omeg*((lst_b_return[j]**beta)-(lst_b_return[i]**beta))

    sumx_b = sumx_b/sumw_b
    tax_pred_b = sumx_b**(1/beta)
    #PRINT OUTCOMES
    print("Lottery A")
    print("Outcomes are - " ,lst_a_return)
    print("Probabilities are - ", lst_a_prob)
    print("Expected payoff is:", ex_payoff_a)
    print("TAX value of Lottery A ", tax_pred_a)
    print("Lottery B")
    print("Outcomes are - " ,lst_b_return)
    print("Probabilities are - ", lst_b_prob)
    print("Expected payoff is:", ex_payoff_b)
    print("TAX value of Lottery B ", tax_pred_b)
    if beta<=1:
        print("Agent type is risk-averse")
    else:
        print("Agent type is risk-seeker")
    if ex_payoff_a>ex_payoff_b:
        print("vNM utility maximiser should choose Lottery A")
    else:
        print("vNM utility maximiser should choose Lottery B")
    if tax_pred_b>tax_pred_a:
        print("As per TAX theory, the DM should choose B")
    else:
        print("As per TAX theory, the DM should choose A")
    return


utility_maximiser()