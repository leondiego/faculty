import numpy as np

def Diagonal(e): # Para el vector de eigenvalores
	d = [];
	n = len(e);
	for i in range(n):
		for j in range(n):
			if j == i:
				d.append(e[i]);
			else:
				d.append(0);
	return np.array(d).reshape(n,n);

A1 = np.array([1/2, 1/2, 0, 1/2, 0, 1/2, 0, 1/2, 1/2]).reshape(3,3);
A2 = np.array([1/2, 1/2, 0, 1/2, 99/200, 1/200, 0, 0, 1]).reshape(3,3);
A3 = np.array([0,1,0,0,0,0,1/5,0,4/5,0,0,0,0,2/5,0,3/5,0,0,0,0,3/5,0,2/5,0,0,0,0,4/5,0,1/5,0,0,0,0,1,0]).reshape(6,6);
A4 = np.array([1,0,0,0,0,0,0.5,0,0.5,0,0,0,0,0.5,0,0.5,0,0,0,0,0.5,0,0.5,0,0,0,0,0.5,0,0.5,0,0,0,0,0,1]).reshape(6,6);
A5 = np.array([1,0,0,0,0,0, 0.5,0,0.5,0,0,0, 0,0.5,0,0.5,0,0, 0,0,0.5,0,0.5,0, 0,0,0,0.5,0,0.5, 0,0,0,0,0,1]).reshape(6,6);

# Para la primera matriz:
e1, P1 = np.linalg.eig(A1);
P1inv = np.linalg.inv(P1); 
D1 = Diagonal(e1);
A1n = np.matmul(P1,np.matmul(np.linalg.matrix_power(D1,100000),P1inv)); # (no se tarda nada en hacer 100,000 multiplicaciones)
Pi0 = np.array([0.4,0.25,0.35]);
PI1 = np.matmul(Pi0, A1n); # Distribución invariante
