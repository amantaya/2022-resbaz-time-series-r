import numpy as np
import matplotlib.pyplot as plt

# Compute the x and y coordinates for points on a sine curve
x = np.arange(0, 3 * np.pi, 0.1) # Uniform array between 0 and 3Pi, with 0.1 spaced points.
y = np.sin(x)

# Plot the points using matplotlib
plt.plot(x, y)
plt.show()  # You must call plt.show() to make graphics appear.

import numpy as np
import matplotlib.pyplot as plt

# Compute the x and y coordinates for points on sine and cosine curves
x = np.arange(0, 3 * np.pi, 0.1)
y_sin = np.sin(x)
y_cos = np.cos(x)

# Plot the points using matplotlib
plt.plot(x, y_sin)
plt.plot(x, y_cos)
plt.xlabel('x axis label')
plt.ylabel('y axis label')
plt.title('Sine and Cosine')
plt.legend(['Sine', 'Cosine'])
plt.show()

import numpy as np
import matplotlib.pyplot as plt

# Compute the x and y coordinates for points on sine and cosine curves
x = np.arange(0, 3 * np.pi, 0.1)
y_sin = np.sin(x)
y_cos = np.cos(x)

# Set up a subplot grid that has height 2 and width 1,
# and set the first such subplot as active.
plt.subplot(2, 1, 1)

# Make the first plot
plt.plot(x, y_sin)
plt.title('Sine')

# Set the second subplot as active, and make the second plot.
plt.subplot(2, 1, 2)
plt.plot(x, y_cos)
plt.title('Cosine')

# Show the figure.
plt.show()

x = np.linspace(0, 5, 100)  # Sample data.

plt.figure(figsize=(10, 6))
plt.grid(True) # Add a grid

plt.plot(x, np.sin(x), label='Sine function $\sin(x)$')  # Plot some data. Can use LaTeX notation.
plt.plot(x, x, label='linear $x$')  # etc.
plt.plot(x, x**3, label='cubic $x^{3}$')
plt.plot(x, x-x**3/6.0, linestyle='dashed', label='$x - x^{3} / 6$')

plt.xlim([0, np.pi/2.0 ]) # We select limits for plotting
plt.ylim([0, 1.5])

# Optional: We can define where and what the xtics can be
plt.xticks([0, 0.125*np.pi, 0.25*np.pi, 0.375*np.pi, np.pi/2], # different as in first method above
           ['$0$', '$\pi/8$','$\pi/4$','$3 \pi/8$','$\pi/2$'])

plt.xlabel('$x$')
plt.ylabel('$y = f(x)$')
plt.title("Simple function plots")
plt.legend(); # the semicolon suppresses any warning messages generated by matplotlib

