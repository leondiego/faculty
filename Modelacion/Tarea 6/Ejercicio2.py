import numpy as np
import timeit
import matplotlib.pyplot as plt
from copy import deepcopy



def LU(A):
    
    m = np.array(A)
    n,n = m.shape
    L = np.diag((1.0)*np.ones(n))
    U = np.matrix.copy(m)
    
    for i in range(n): 
        for j in range (1, n-i):
            L[i+j, i] = U[i+j,i]/U[i,i]
            U[i+j,:] = U[i+j,:] - (U[i+j,i]/U[i,i])*U[i,:]
    return L,U


def QR(A):
    if np.linalg.det(A) != 0: #comprobar que tenga solución
        n,n = A.shape
        Q=np.zeros((n,n))
        u=A[:,0]
        e=u/np.linalg.norm(u)
        Q[:,0]=e
        for i in range(1,n):
            u=A[:,i]
            for j in range(i):
                aux=np.dot(A[:,j+1],Q[:,j])*Q[:,j]
                u=u-aux
            e=u/np.linalg.norm(u)
            Q[:,i]=e
        R=np.matmul(Q.transpose(),A)
    return Q,R



def tomar_tiempo(N,n):
    tiempoLU=[]
    tiempoQR=[]
    for i in range(N):
        #Que tome valores entre -1,1
        A=np.random.uniform(-1,1,(n,n))
        b=np.random.uniform(-1,1,(n,1))

        #tomar_tiempo a LU
        tempo1 = timeit.default_timer()
        L,U=LU(A)
        b1=np.dot(np.linalg.inv(L),b)
        x=np.dot(np.linalg.inv(U),b1)
        tempo2 = timeit.default_timer()
        tiempoLU.append(tempo2-tempo1)
        
        #tomar_tiempo a QR
        tempo3 = timeit.default_timer()
        Q,R=QR(A)
        b1=np.dot(np.linalg.inv(Q),b)
        x=np.dot(np.linalg.inv(R),b1)
        tempo4 = timeit.default_timer()
        tiempoQR.append(tempo4-tempo3)
    l=promedio(tiempoLU)
    m=promedio(tiempoQR)
    return tiempoLU, tiempoQR, l,m
        
   
def promedio(l):
    return sum(l)/len(l)
 



l=[]
m=[]
for i in range(1,10):
    c=tomar_tiempo(i,3)
    l.append(c[2])
    m.append(c[3])
l3=promedio(l)
m3=promedio(m)


for i in range(1,30):
    c=tomar_tiempo(i,5)
    l.append(c[2])
    m.append(c[3])
l5=promedio(l)
m5=promedio(m)

l=[]
m=[]
for i in range(1,30):
    c=tomar_tiempo(i,10)
    l.append(c[2])
    m.append(c[3])
l10=promedio(l)
m10=promedio(m)
    
l=[]
m=[]
for i in range(1,30):
    c=tomar_tiempo(i,15)
    l.append(c[2])
    m.append(c[3])
l15=promedio(l)
m15=promedio(m)

l=[]
m=[]
for i in range(1,30):
    c=tomar_tiempo(i,30)
    l.append(c[2])
    m.append(c[3])
l30=promedio(l)
m30=promedio(m)

l=[]
m=[]
for i in range(1,30):
    c=tomar_tiempo(i,40)
    l.append(c[2])
    m.append(c[3])
l40=promedio(l)
m40=promedio(m)

l=[]
m=[]
for i in range(1,30):
    c=tomar_tiempo(i,50)
    l.append(c[2])
    m.append(c[3])
l50=promedio(l)
m50=promedio(m)

l=[]
m=[]
for i in range(1,30):
    c=tomar_tiempo(i,75)
    l.append(c[2])
    m.append(c[3])
l75=promedio(l)
m75=promedio(m)

plt.plot([3,5,10,20,40,60,80,100],[l3,l5,l10,l20,l40,l60,l80,l100],color='magenta',label='LU')
plt.plot([3,5,10,20,40,60,80,100],[m3,m5,m10,m20,m40,m60,m80,m100],color='',label='QR')
plt.title('Comparación')
plt.xlabel('n (Mnxn)')
plt.ylabel('Tiempo')
plt.legend()
plt.legend()
plt.show()
    
    