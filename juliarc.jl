import Base.reshape

reshape{T}(a, dims::Array{T})  = reshape(a, tuple(dims...))

function rshp( arr, dims...)
    """
    Reshape an array like in numpy or matlab

    dims is a list of new dimensions. dims can contain a single instance of -1
    as a placeholder for the correct number of elements. 

    Example:

    julia> rshp([1:10], 2,-1)
    2x5 Array{Int64,2}:
     1  3  5  7   9
     2  4  6  8  10

    """
    nel = length(arr)

    dims = [int64(a) for a in dims]

    nelsh = one(dims[1])
    fillme = 0

    for (i, d) in enumerate(dims)
        if d == -1 
            fillme = i
        else
            nelsh *= d
        end
    end

    if fillme > 0 
        dims[fillme] = nel / nelsh
    end

    Base.reshape( arr,  dims)
end 
