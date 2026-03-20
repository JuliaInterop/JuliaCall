if VERSION < v"0.6.5"
    Base.load_juliarc()
else
    Base.load_julia_startup()
    using Pkg
    ## needed by console
    const STDIN = stdin
    ## needed by rmarkdown stdout capture
    const STDOUT = stdout
    ## needed by functions like det
    using LinearAlgebra
    ## needed by functions like mean
    using Statistics
end

module JuliaCall

const julia07 = VERSION > v"0.6.5"
const julia14 = VERSION > v"1.3.2"

if julia07
    using Pkg
    ## needed by eval_string function
    const parse = Meta.parse
    ## needed by system checking
    const is_windows = Sys.iswindows
    ## needed by display system
    const Display = AbstractDisplay
    const readstring(s) = read(s, String)
    ## needed by console
    using REPL
    const REPLCompletions = REPL.REPLCompletions

    ## needed by round and signif
    const round1(x::Number, digits) = round(x; digits=digits)
    const signif(x::Number, digits) = round(x; sigdigits=digits)

    ## needed by IRjulia display
    using Base64
else
    ## in julia06
    ## needed by console
    const REPL = Base.REPL
    const REPLCompletions = Base.REPLCompletions

    ## needed by round
    const round1 = round
end

function installed(name)
    @static if julia14
        for p in values(Pkg.dependencies())
            if p.name == name && p.is_direct_dep
                return p.version
            end
        end
        nothing
    elseif julia07
        get(Pkg.installed(), name, nothing)
    else
        Pkg.installed(name)
    end
end

# gc_enable(false)

# Pkg.update()

# if Pkg.installed("RCall") == nothing Pkg.add("RCall") end

using Suppressor

if !is_windows() && !julia07
    @suppress_err begin
        @eval Base JULIA_HOME = joinpath(dirname(JULIA_HOME), "bin")
        @eval Base julia_cmd() = julia_cmd(joinpath(JULIA_HOME, julia_exename()))
    end
end

using RCall

## Fix https://github.com/JuliaInterop/RCall.jl/issues/289
unsafe_store!(cglobal((:R_CStackLimit,RCall.libR),Csize_t), typemax(Csize_t))

const need_display = length(Base.Multimedia.displays) < 2

import Base.Multimedia.display
if need_display
    include("./display/basic.jl")
end
include("./display/IRjulia.jl")
include("./display/RmdJulia.jl")
include("./display/plotsViewer.jl")
include("REPLhook.jl")
include("incomplete_console.jl")
include("RmdStd.jl")

include("convert.jl")
include("JuliaObject.jl")
include("asR.jl")
include("dispatch.jl")

function error_msg(e, bt)
    m = IOBuffer()
    showerror(m, e, bt)
    seek(m, 0)
    readstring(m)
end

function Rerror(e, bt)
    s1 = join(["Error happens in Julia.\n"])
    s2 = error_msg(e, bt)
    s = join([s1 s2])
    rcall(:simpleError, s)
end

function funcfind(name)
    r = Main
    ns = split(name, ".")
    for n in ns
        r = getfield(r, Symbol(n))
    end
    r
end

# function call_decompose(call1)
#     call = rcopy(RObject(Ptr{RCall.VecSxp}(call1)))
#     (call[:fname], call[:named_args], call[:unamed_args], call[:need_return], call[:show_value])
# end

function call_decompose(call1)
    call = Ptr{RCall.VecSxp}(call1)
    # fname = rcopy(String, call[:fname])
    fname = rcopy(String, call[1]) :: String
    # named_args = Any[(rcopy(Symbol, k), rcopy(i)) for (k, i) in enumerate(call[:named_args])]
    # unamed_args = Any[rcopy(i) for i in call[:unamed_args]]
    args = call[2] :: Ptr{RCall.VecSxp}
    symbols = rcopy(Vector{Symbol}, getnames(args)) :: Vector{Symbol}
    es = rcopy(Vector{Any}, args) :: Vector{Any}
    if length(symbols) == 0
        unamed_args = es
        named_args = Any[]
    else
        unamed_args = Any[es[i] for i in 1:length(es) if symbols[i] == Symbol("")]
        named_args = Any[(symbols[i], es[i]) for i in 1:length(es) if symbols[i] != Symbol("")]
    end

    # named_args = Any[]
    # unamed_args = Any[]
    # # for (k,a) in enumerate(call[:args])
    # for (k,a) in enumerate(args)
    #     if isa(k.p, Ptr{RCall.NilSxp})
    #         push!(unamed_args, rcopy(a))
    #     else
    #         push!(named_args, (rcopy(Symbol,k), rcopy(a)))
    #     end
    # end
    # need_return = rcopy(String, call[:need_return])
    need_return = rcopy(String, call[3]) :: String
    # show_value = rcopy(Bool, call[:show_value])
    show_value = rcopy(Bool, call[4]) :: Bool
    (fname, named_args, unamed_args, need_return, show_value)
end

const _interrupt_timer = Ref{Union{Timer, Nothing}}(nothing)
const _main_task = Ref{Union{Task, Nothing}}(nothing)

"""
    r_interrupt_pending()::Bool

Check whether R has a pending user interrupt (Ctrl+C / Stop button)
by reading the R-internal flag directly via cglobal.
Returns true if the flag is set. Does NOT reset the flag — we leave
it set so R handles the interrupt natively when control returns.
"""
function r_interrupt_pending()
    # We read R's interrupt-pending flag directly via cglobal, the same way
    # RCall.jl does in its eventloop.jl:
    # https://github.com/JuliaInterop/RCall.jl/blob/6c76130/src/eventloop.jl#L16-L23
    # On Windows the flag is called UserBreak; on Unix R_interrupts_pending.
    @static if Sys.iswindows()
        ptr = cglobal((:UserBreak, RCall.libR), Cint)
    else
        ptr = cglobal((:R_interrupts_pending, RCall.libR), Cint)
    end
    return unsafe_load(ptr) != 0
end

function start_interrupt_monitor(; interval = 0.2)
    stop_interrupt_monitor()
    _main_task[] = current_task()
    maintask = _main_task[]
    _interrupt_timer[] = Timer(0.0; interval = interval) do t
        if r_interrupt_pending()
            close(t)
            # Throw the InterruptException into the main task so the
            # actual computation (not just the timer) gets interrupted.
            Base.throwto(maintask, InterruptException())
        end
    end
end

function stop_interrupt_monitor()
    timer = _interrupt_timer[]
    if timer !== nothing
        close(timer)
        _interrupt_timer[] = nothing
    end
    _main_task[] = nothing
end

function docall(call1)
    start_interrupt_monitor()
    try
        fname, named_args, unamed_args, need_return, show_value = call_decompose(call1);
        if endswith(fname, ".")
            fname = chop(fname);
            # f = eval(Main, parse(fname));
            f = funcfind(fname);
            r = broadcast(f, unamed_args...);
        else
            # f = eval(Main, parse(fname));
            f = funcfind(fname);
            r = f(unamed_args...; named_args...);
        end
        if show_value && r != nothing
            display(r)
        end
        # @static if need_display
        #     proceed(basic_display_manager)
        # end
        if need_return == "R"
            sexp(r);
        elseif need_return == "Julia"
            sexp(JuliaObject(r));
        else
            sexp(nothing);
        end;
    catch e
        if e isa InterruptException
            # Return a harmless value on interrupt.
            # R_interrupts_pending is still set, so R will handle the
            # interrupt itself once control returns from .Call.
            sexp(nothing);
        else
            Rerror(e, stacktrace(catch_backtrace())).p;
        end;
    finally
        stop_interrupt_monitor()
    end;
end

include("interface1.jl")

function exists(x)
    isdefined(Main, Symbol(x))
end

function eval_string(x)
    Core.eval(Main, parse(strip(x)))
end

function installed_package(pkg_name)
    try
        string(installed(pkg_name))
    catch e
        "nothing";
    end
end

function help(fname)
    string(eval_string(join(["@doc " fname])))
end

function assign(name, x)
    Core.eval(Main, Expr(:(=), Symbol(name), x))
end

function str_typeof(x)
    string(typeof(x))
end

function show_string(x)
    buf = IOBuffer()
    show(IOContext(buf, :limit=>true), x)
    return String(take!(buf))
end

## Needed by julia_source
function include1(fname)
    @static if julia07
        Base.include(Main, fname)
    else
        include(fname)
    end
end

# function apply(f, args...)
#     f(args...)
# end

function apply(f, unamed_args...; named_args...)
    f(unamed_args...; named_args...)
end

end
