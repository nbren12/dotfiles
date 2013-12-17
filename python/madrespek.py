def pgram(x, fs=1.0):
    from scipy import signal
    from scipy import fft
    ps  = abs(fft.fft(x**2))**2
    fs  = fft.fftfreq(x.shape[0],fs)

    return semilogx(ps,fs)
