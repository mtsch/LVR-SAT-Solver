using LightGraphs
using GraphIO

function createcnf(g, ncolors)
    colors = 1:ncolors

    getcolor(v, c) = (v - 1) * ncolors + c

    clauses = String[]
    # Vertex clauses.
    for v in vertices(g)
        vars = [-getcolor(v, c) for c in colors]
        for c in colors
            vars[c] *= -1
            push!(clauses, join(map(string, vars), ' ') * " 0")
            vars[c] *= -1
        end
    end

    for e in edges(g)
        v1 = src(e)
        v2 = dst(e)
        for c in colors
            push!(clauses, "$(-getcolor(v1, c)) $(-getcolor(v2, c)) 0")
        end
    end

    nvars = nv(g) * ncolors
    header = "p cnf $nvars $(length(clauses))\n"

    header * join(clauses, '\n') * "\n"
end

function fromfile(ncolors, infile, outfile, fmt=NETFormat())
    g = loadgraph(infile, "g", fmt)
    formula = createcnf(g, ncolors)
    write(outfile, formula)
end
