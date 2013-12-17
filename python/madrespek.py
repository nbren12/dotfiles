import matplotlib.pyplot as plt


def pgram(x, fs=1.0):
    """
    (C) Noah D. Brenowitz
    
    A function to plot the periodogram of a signal x
    """
    from numpy import fft
    ps  = abs(fft.fft(x**2))**2
    fs  = fft.fftfreq(x.shape[0],fs)

    return plt.semilogx(fs, ps)
