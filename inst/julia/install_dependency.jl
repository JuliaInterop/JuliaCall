## Thanks to randy3k for pointing this out,
## `RCall` needs to be precompiled with the current R.
## <https://github.com/Non-Contradiction/JuliaCall/issues/9>
## as well as coming up with the solution

CurrentRhome = normpath(ARGS[1])

## println(Rhome)

ENV["R_HOME"] = CurrentRhome

const julia07 = VERSION > v"0.6.5"
const julia14 = VERSION > v"1.3.2"

if julia07
    using Pkg
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

if installed("Suppressor") == nothing
    # Use try-catch to handle potential package conflicts on ARM64
    try
        Pkg.add("Suppressor")
    catch e
        @warn "Failed to install Suppressor directly, trying to resolve in isolated environment" exception=e
        # Create a temporary isolated environment to avoid conflicts
        temp_env = mktempdir()
        Pkg.activate(temp_env)
        Pkg.add("Suppressor")
        Pkg.activate() # Return to default environment
        # Now try again in the main environment
        Pkg.add("Suppressor")
    end
end;

using Suppressor

if installed("RCall") == nothing
    # Use try-catch to handle potential package conflicts on ARM64
    try
        Pkg.add("RCall")
    catch e
        @warn "Failed to install RCall directly, trying to resolve in isolated environment" exception=e
        # For RCall, we need to be more careful as it needs to be in the main environment
        # Try to update registry first which often fixes ARM64 issues
        try
            Pkg.Registry.update()
        catch
            @warn "Could not update registry"
        end
        # Try again after registry update
        Pkg.add("RCall")
    end
end;
