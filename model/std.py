import numpy as np

def std(x):
    """
    Examples
    -----------
    >>> import numpy as np
    >>> x = np.array([1,2,3,4,5,np.NaN,7])
    >>> np.nanmean(x)
    3.6666666666666665
    >>> np.nanvar(x)
    3.8888888888888893
    >>> np.nanvar(x, ddof=1)
    4.666666666666667
    >>> np.nanstd(x)
    1.9720265943665387
    >>> std(x)
    array([-1.35224681, -0.84515425, -0.3380617 ,  0.16903085,  0.6761234 ,
                   nan,  1.69030851])

    Sklearn alternative

    >>> from sklearn.preprocessing import StandardScaler
    >>> scale = StandardScaler()
    >>> scale.fit_transform(x.reshape(-1,1))
    array([[-1.35224681],
           [-0.84515425],
           [-0.3380617 ],
           [ 0.16903085],
           [ 0.6761234 ],
           [        nan],
           [ 1.69030851]])

    Another sklearn alternative

    >>> from sklearn import preprocessing
    >>> preprocessing.scale(x.reshape(-1,1))
    array([[-1.35224681],
           [-0.84515425],
           [-0.3380617 ],
           [ 0.16903085],
           [ 0.6761234 ],
           [        nan],
           [ 1.69030851]])
    """
    return (x - np.nanmean(x))/np.nanstd(x)

