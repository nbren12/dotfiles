import matplotlib.pyplot as plt


def pgram(x, fs=1.0):
    """
    A function to plot the periodogram of a signal x

    (C) 2013 Noah D. Brenowitz
    """
    from numpy import fft
    ps  = abs(fft.fft(x**2))**2
    fs  = fft.fftfreq(x.shape[0],fs)

    return plt.semilogx(fs, ps)

def scatter_line_plot(x, y, bands = None, alpha=.05):
    """
    Scatter plot with lines

    (C) 2013 Noah D. Brenowitz
    """
    from numpy import polyfit, polyval

    plt.plot(x, y, '.', alpha=alpha)

    if bands is not None:
        for band in bands:
            ind = (x > band[0]) &( x < band[1])
            pf = polyfit( x[ind], y[ind], 1)
            plt.plot(x[ind], polyval(pf, x[ind]))

    else:
        pf = polyfit(x, y, 1)
        plt.plot(x , polyval(pf, x))

def cart2pol(x,y):
    from pylab import sqrt, arctan, pi
    r = sqrt(x**2 + y**2)
    theta = arctan(y/x)
    theta[x<0] = pi + theta[x<0]
    theta[theta <0 ] = theta[theta<0] +2*pi

    return (r, theta)
