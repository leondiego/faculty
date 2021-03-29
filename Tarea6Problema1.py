import numpy as np
import time
import matplotlib.pyplot as plt

def matrizmenor(A,i,j): # Sólo para matrices cuadradas
	m,n = A.shape;
	if m > 1:
		B1 = A[0:i,0:j];
		B2 = A[0:i,j + 1:];
		B3 = A[i + 1:,0:j];
		B4 = A[i + 1:,j + 1:];
	B5 = np.concatenate((B1,B2),axis = 1);
	B6 = np.concatenate((B3,B4),axis = 1);
	B = np.concatenate((B5,B6));
	return B

def determinante(A): # Función que obtiene el determinante de una matriz
	n,n = A.shape;
	if n == 2:
		d = A[0,0] * A[1,1] - A[0,1] * A[1,0];
	else:
		oop = 0;
		for i in range(0,n):
			oop = oop + ((-1) ** (i)) * A[0,i] * determinante(matrizmenor(A,0,i));
		d = oop;
	return d

def NMatrices(n,N): # Genera N matrices de nxn
	M = [];
	for k in range(N):
		M.append([[np.random.uniform(-1,1) for j in range(n)] for i in range (n)]);
	M = np.asarray(M);
	return M

def TomarTiempo(M,k): # Función que obtiene los valores t(i), desde i = 1 hasta i = k
	t = [];
	for i in range(k):
		start = time.time();
		determinante(M[0,:,:]);
		for j in range(1,i + 1):
			determinante(M[j,:,:]);
		end = time.time();
		t.append(end - start);
	return t

#######################################################################################################################################################

# Inciso a):
# Fijamos N = 150
N = 20;

# Inciso b):
nmax = 40;
def promedios(nmax,N): # Esta función obtiene los promedios m(n) desde n = 2 hasta n = N
	prom = [];
	for i in range(1,nmax):
		M = NMatrices(n + 1,N); # Empezaremos con n = 2 ya que la función para calcular el determinante no está definida para matrices de una dimensión y de todas formas su determinante sería el número mismo.
		t = TomarTiempo(M,N);
		mn = sum(t)/N;
		prom.append(mn);
	return prom

# Inciso c):
prom = promedio(nmax,N); # Se fueron cambiando los parámetros, estos no fueron los únicos, pero tardaron mucho en calcularse
plot.plot(range(2,41),prom,'ro-');
plt.title('nmax = 40, N = 20');
plt.xlabel('n');
plt.ylabel('m(n)');
plt.legend();
plt.show();
