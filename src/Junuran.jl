module Junuran

import Libdl
export urgen_vnrou
export ursample


mutable struct UNUR_DISTR
end
mutable struct UNUR_PAR
end
mutable struct UNUR_GEN
end


function urgen_vnrou(
  dim::Integer, 
  pdf::Function, 
  center::Union{Nothing,Vector{Float64}} = nothing,
  mode::Union{Nothing,Vector{Float64}} = nothing,
  lower::Union{Nothing,Vector{Float64}} = nothing,
  upper::Union{Nothing,Vector{Float64}} = nothing
)

  # Open the UNU.RAN library 
  lib = Libdl.dlopen("/usr/local/lib/libunuran") 

  # checks
  if dim < 2
    error("zzz")
  end

  if center !== nothing && length(center) != dim
    error("zzz")
  end

  if mode !== nothing && length(mode) != dim
    error("zzz")
  end

  if lower !== nothing && length(lower) != dim
    error("zzz")
  end

  if upper !== nothing && length(upper) != dim
    error("zzz")
  end

  # create continuous multivariate distribution
  distr = ccall(
    Libdl.dlsym(lib, :unur_distr_cvec_new), 
    Ptr{UNUR_DISTR}, 
    (Cint, ), 
    dim
  )

  # define the unnormalized pdf of the distribution
  pdf_j = function(xptr, distr)
    x = map(i -> unsafe_load(xptr, i), 1:dim)
    return pdf(x)::Cdouble
  end
  pdf_c = @cfunction(
    $pdf_j,
#    (xptr, distr) -> pdf(map(i -> unsafe_load(xptr, i), 1:dim))::Cdouble, 
    Cdouble, 
    (Ptr{Cdouble}, Ptr{UNUR_DISTR})
  )

  # set the pdf
  ccall(
    Libdl.dlsym(lib, :unur_distr_cvec_set_pdf), 
    Cvoid,
    (Ptr{UNUR_DISTR}, Ptr{Cvoid}),
    distr, pdf_c
  )

  # set center
  if center !== nothing
    ccall(
      Libdl.dlsym(lib, :unur_distr_cvec_set_center), 
      Cvoid,
      (Ptr{UNUR_DISTR}, Ref{Cdouble}),
      distr, center
    )
  end

  # set mode
  if mode !== nothing
    ccall(
      Libdl.dlsym(lib, :unur_distr_cvec_set_mode), 
      Cvoid,
      (Ptr{UNUR_DISTR}, Ref{Cdouble}),
      distr, mode
    )
  end

  # create the parameters object
  par = ccall(
    Libdl.dlsym(lib, :unur_vnrou_new), 
    Ptr{UNUR_PAR},
    (Ptr{UNUR_DISTR}, ),
    distr
  )

  # set the rectangular domain
  if lower !== nothing && upper !== nothing
    ccall(
      Libdl.dlsym(lib, :unur_vnrou_set_u),
      Cvoid,
      (Ptr{UNUR_PAR}, Ref{Cdouble}, Ref{Cdouble}),
      par, lower, upper
    )
  end

  # create the generator
  gen = ccall(
    Libdl.dlsym(lib, :unur_init), 
    Ptr{UNUR_GEN},
    (Ptr{UNUR_PAR}, ),
    par
  )

  # TODO: check gen successful (i.e. not the null pointer)

  # destroy the distribution object
  ccall(
    Libdl.dlsym(lib, :unur_distr_free), 
    Cvoid,
    (Ptr{UNUR_DISTR}, ),
    distr
  )

  # output
  return (generator = gen, type = "cmv", dim = dim, unuran = lib)

end # urgen_vnrou


function ursample(
  urgen, 
  n::Integer,
  seed::Union{Nothing,Integer} = nothing
) 

  if seed !== nothing
    seed = convert(UInt32, abs(seed) + 1)
  end
  # set generator seed
  ccall(
    Libdl.dlsym(urgen.unuran, :unur_gen_seed),
    Cint,
    (Ptr{UNUR_GEN}, Culong),
    urgen.generator, seed === nothing ? rand(UInt32) + UInt32(1) : seed
  )

  out = Vector{Vector{Cdouble}}(undef, n)
  for i in 1:n
    out[i] = Vector{Cdouble}(undef, urgen.dim)
    ccall(
      Libdl.dlsym(urgen.unuran, :unur_sample_vec), 
      Cint,
      (Ptr{UNUR_GEN}, Ref{Cdouble}),
      urgen.generator, out[i]
    )
  end
#=  ccall(
    Libdl.dlsym(urgen.unuran, :unur_reinit), 
    Cint,
    (Ptr{UNUR_GEN}, ),
    urgen.generator
  )
=#
  return out
end # ursample


end # module
