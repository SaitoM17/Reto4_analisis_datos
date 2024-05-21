import matplotlib.pyplot as plt

data = {
    'HC': [44.9, 45.6, 45.4, 44.9, 44.3],
    'HI': [39.2, 47, 50.9, 52.9, 56.4],
    'HT': [93.5, 93.1, 93.2, 92.9, 92.5],
    'HTP': [43.7, 52.1, 49.5, 47.3, 45.9],
    'U6E': [51.3, 47, 45.3, 45, 43],
    'UI6E': [57.4, 59.5, 63.9, 65.8, 70.1],
    'UCHE': [51.3, 52.2, 46.8, 46.7, 44.6],
    'UITI': [12.8, 14.7, 20.4, 23.7, 27.2],
    'UIFH': [29.1, 20.5, 16.7, 13.4, 10.7],
    'UTC6E': [71.5, 73.6, 72.2, 73.5, 75.1]
}

alfa = 0.2
valores_suavi = {}

for llave, valores in data.items():
    valor_nuevo = alfa * valores[-1] + (1 - alfa) * valores[-2]
    valores_suavi[llave] = valor_nuevo
    data[llave].append(valor_nuevo)

for llave, valor in valores_suavi.items():
    print(f"{llave} para el 2020: {valor:.2f}")

annos = [2015, 2016, 2017, 2018, 2019, 2020]

plt.figure(figsize=(15, 8))
for llave, valores in data.items():
    plt.plot(annos, valores, marker='o', label=llave)

plt.xlabel('Año')
plt.ylabel('Valor del Indicador')
plt.legend()
plt.grid(True)

plt.show()