import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from math import sqrt


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
    """
    Cartesian coordinates to polar

    (C) 2013 Noah D. Brenowitz
    """
    from pylab import sqrt, arctan, pi
    r = sqrt(x**2 + y**2)
    theta = arctan(y/x)
    theta[x<0] = pi + theta[x<0]
    theta[theta <0 ] = theta[theta<0] +2*pi

    return (r, theta)


def acf(x, axes=(0,1)):
    """
    2D ACF using fft

    Inputs:

    x is ndarray with shape (nt, nx)
    """
    from numpy.fft import fft2, ifft2, fftshift

    if x.ndim == 1:
        x = x[:,None]
    elif x.ndim == 2:
        pass
    else:
        raise NotImplementedError

    nt, nx = x.shape

    padding = np.zeros((nt, nx))

    x = np.concatenate((x, padding))

    fx = fft2(x, axes=axes)
    ac=  np.real(ifft2(fx * np.conj(fx), axes=axes))[:(nt-10),:] / nx / np.arange(nt, 10, -1)[:,None]

    ac = ac[:nt/2, :nx/2]

    return ac

def pdacf(y, nlags=1000):
    import statsmodels.api as sm
    ac = sm.tsa.acf(y, True, nlags=nlags, fft=True)
    t  = np.array(y.index[:nlags+1]) -y.index[0]
    return pd.Series( ac, t )

def hovmoller(cube, tmin, tmax, xmin=0, xmax=360, demean = True, cmap = 'PuOr_r', **kwargs):
    import iris
    import iris.quickplot as qplt
    import iris.plot as iplt
    if demean:
        mu   = cube.collapsed(('time' ,), iris.analysis.MEAN)
        plotme = cube.extract(iris.Constraint(time = lambda cell: tmin < cell < tmax))
        plotme.data -= mu.data[None,...]
    else:
        plotme = cube.extract(iris.Constraint(time = lambda cell: tmin < cell < tmax))

    plotme = plotme.extract(iris.Constraint(longitude = lambda x : xmin < x < xmax))

    im = qplt.contourf(plotme, 20, extend='both', cmap = cmap, **kwargs)
    title = qplt._title(cube, False)
    std = sqrt(cube.collapsed('time', iris.analysis.VARIANCE).collapsed('longitude', iris.analysis.MEAN).data)
    m   = cube.collapsed(('time' ,'longitude'), iris.analysis.MEDIAN).data
    plt.gca().set_title('%s Rms %.1f Med %.1f'%(title,  std, m))
    plt.gca().axis('tight')

def climatology(cube):
    """
    Summary hovmoller diagrams for various fields in the netcdf file
    """
    import iris
    import iris.quickplot as qplt
    import iris.plot as iplt
    import matplotlib.pyplot as plt
    from math import sqrt


    mu = cube.collapsed(('time',) , iris.analysis.MEAN)
    mmu = mu.collapsed(('time', 'longitude'), iris.analysis.MEAN)
    std = cube.collapsed(('time',) , iris.analysis.STD_DEV)
    qplt.plot(mu)
    iris.plot.plot(mu + std, 'r--')
    iris.plot.plot(mu - std,'r--')
    ax=  plt.gca().twinx()
    iris.plot.plot(std ,'k-')

    title = qplt._title(cube, False)
    plt.gca().set_title(title)
    plt.gca().axis('tight')

def wk_plot(cube):
    from scipy.fftpack import fft2, fftfreq, fftshift
    from matplotlib import mlab
    z = np.squeeze(cube.data)
    z = mlab.demean(z)

    x = cube.coord('longitude').points
    t = cube.coord('time').points

    nt, nx = z.shape
    dt = np.diff(t).mean()
    dx = 1.0 / nx

    fz = fftshift(np.abs(fft2(z)), axes=1)
    fx = fftshift(fftfreq(nx, d = dx ))
    ft = fftfreq(nt, d = dt)

    pz = np.abs(fz)**2/(nx * nt)**2 *2

    nt2 = nt/2

    levs=  np.arange(-10, -2, .5)


    plt.contourf(fx, ft[:nt2], np.log10(pz[:nt2, :]), levs, extend='both')
    plt.colorbar()



    def plot_symmetric_sw_waves(h, type='cpd'):
        c = np.sqrt(9.81 * h) * 86400 / 4e7
        plt.plot( fx, fx * c, 'k')

    plot_symmetric_sw_waves(25)
    plot_symmetric_sw_waves(250)

    plt.grid('on')
    plt.xlim([-15, 15])
    plt.ylim([0, min(.8, ft.max())])
    plt.xlabel('Zonal Wavenumber')
    plt.ylabel('CPD')


