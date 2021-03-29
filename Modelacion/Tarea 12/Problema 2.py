import numpy as np
import networkx as nx
import matplotlib.pyplot as plt

# Creamos la gráfica a partir de los datos:
G = nx.read_edgelist(r'/home/diego/GIT/faculty/Modelación/Twitter_network_R.csv', delimiter = ',', create_using = nx.DiGraph(), nodetype = str);

usuarios = np.array(G.nodes());
adyacencia = nx.to_numpy_matrix(G);

# Vamos a contar seguidores y seguidos de cada usuario:
seguidores = [sum([adyacencia[i,j] for i in range(34)]) for j in range(34)];
seguidos = [sum([adyacencia[i,j] for j in range(34)]) for i in range(34)];

# Creamos la matriz de transición a partir de la de adyacencia y las indicaciones de la tarea:
transicion = np.zeros((34,34));
for i in range(34):
	sumafila = sum([adyacencia[i,k] for k in range(34)]);
	print(i);
	print(sumafila);
	for j in range(34):
		if sumafila == 0:
			transicion[i,j] = 1 / 34; # Si el vértice no tiene aristas salientes, el caminante es teletransportado a cualquier nodo de la gráfica.
		if adyacencia[i,j] == 1 and sumafila != 0:
			transicion[i,j] = 1 / sumafila;

# Simulamos el proceso de Markov:
Xn = [];
ultimo = list(np.random.choice(a = range(34),size = 1))[0]; # Asumimos distribución inicial uniforme.
for i in range(100000):
	Xn.append(usuarios[ultimo]);
	ultimo = list(np.random.choice(a = range(34),size =  1, p = list(transicion[ultimo,:])))[0];

# Contamos:
conteo = [Xn.count(usuarios[i]) for i in range(34)];

############################# Gráficos

# Gráfica:
nx.draw(G, with_labels = 1, node_color = 'steelblue', edge_color = 'mediumturquoise');
plt.show();

# Subgráfica:
subG = G.subgraph(usuarios[5:25]);
nx.draw(subG, with_labels = 1, node_color = 'limegreen', edge_color = 'springgreen');
plt.show();

# Histograma:
plt.hist(Xn, bins = 34, color = 'gold', ec = 'yellow');
plt.title('X_n');
plt.ylabel('Frecuencia');
plt.xticks(rotation = 'vertical');
plt.show();

# Gráfica con tamaño de nodos por importancia:
nx.draw(G, with_labels = 1, node_size = conteo, node_color = 'hotpink', edge_color = 'pink');
plt.show();
