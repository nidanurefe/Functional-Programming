# Shelling Segregation model

## Instructions 

# Source: Thomas C. Schelling (1971) Dynamic models of segregation, The Journal of Mathematical Sociology, 1:2, 143-186, DOI: [10.1080/0022250X.1971.9989794](https://doi.org/10.1080/0022250X.1971.9989794) 

# In this exercise you will implement the [Shelling Segregation Model](https://en.wikipedia.org/wiki/Schelling%27s_model_of_segregation), a classic agent-based model in the social sciences, which introduces the concepts of emergent macro-behaviours and tipping points, and which led to its author (Thomas Schelling) being awarded the Nobel Prize in Economics in 2005.

# It was one of the very first results obtained from running simulations, especially in the social sciences.
# The world is modelled as a gridded space inhabited by two groups (at the time, racial and segregation issues concerning the co-existence of "blacks" and "whites" were topical).
# Each group has a preference to live in a neighbourhood inhabited by agents of its own type, with a certain "tolerance". If the proportion of agents of its own type in the neighbourhood falls below this "tolerance" level, the agent will try to move to a place with a higher proportion of agents of its own type.
# The main result of the paper was that even a relatively mild preference for the presence of similar agents would have led to a segregated world, and that there was a specific threshold (between 30% and 40%) that drove these two completely different outcomes, i.e. a "tipping point".

# The simulation algorithm then works like this:
# At each step, it looks at each agent, checks whether it is "happy" with its current location (looking at the proportion of its own types in the neighbourhood), and if not, moves the agent to a position where it would be (setting its previous location as empty).

# There are several "generality" vs. "specificity" ways to code the above algorithm. On the one hand you could hard code the two agent types, e.g. as `blue` and `red`, on the other hand you could be very generic and create an abstract type `agent` and a concrete class for each agent type.

# The skeleton below suggests an intermediate approach with only one `Agent` class and the type of agent encoded as an integer, where 0 represents an empty cell. Feel free to use it or develop your own algorithm from scratch!

# Source: Thomas C. Schelling (1971) Dynamic models of segregation, The Journal of Mathematical Sociology, 1:2, 143-186, DOI: [10.1080/0022250X.1971.9989794](https://doi.org/10.1080/0022250X.1971.9989794) 



cd(@__DIR__)         
using Pkg             
Pkg.activate(".")   
Pkg.instantiate()
using Random
Random.seed!(123)
using Plots

mutable struct ShellingAgent
    gid::Int64
end
mutable struct Env
    nR::Int64                       # number of rows
    nC::Int64                       # number of columns
    similarityThreeshold::Float64   # threeshold for agents to be "happy" with their location
    neighborhood::Int64             # how far looking for "similar" agents
    nSteps::Int64                   # number of iteractive steps to employ
    cells::Vector{ShellingAgent}            # total cells in the environment
    gids::Vector{Int64}             # ids of the agents types (or "groups")
    grsizes::Vector{Int64}          # number of agents per group
end

xyToId(x,y,nR,nC) =  nR*(x-1)+y # Alternative approach: use CartesianIndexes
iDToXY(id,nR,nC)  =  Int(floor((id-1)/nR)+1), (id-1)%(nR)+1
printableGrid(env) = reshape([a.gid for a in env.cells],env.nR,env.nC)

function getNeighbours(x,y,env,gid=nothing)
    board  = reshape(env.cells,env.nR,env.nC)
    region = board[max(1,y-env.neighborhood):min(nR,y+env.neighborhood),max(1,x-env.neighborhood):min(nC,x+env.neighborhood)]
    if gid == nothing
        return sum(getproperty.(region, :gid) .!= 0) # return all agents that are not zero
    else
        return sum(getproperty.(region, :gid) .== gid)
    end
end

function save_cells_to_txt(cells, filename)
    open(filename, "w") do f
        for agent in cells
            println(f, agent.gid)
        end
    end
end

function isHappy(x,y,a,env)
    totalNeighbours  = getNeighbours(x,y,env)
    myTypeNeighbours = getNeighbours(x,y,env,a.gid)
    return myTypeNeighbours/totalNeighbours > env.similarityThreeshold
end


function happy_perc(env)
    happyCount = 0
    for (i,a) in enumerate(env.cells)
        gid = a.gid
        if gid == 0 continue; end # Now I would have written it as gid == 0 && continue
        (x,y) = iDToXY(i,env.nR,env.nC)
        if isHappy(x,y,a,env)
            happyCount += 1
        end
    end
    return happyCount/sum(env.grsizes)
end

nR         = 200
nC         = 200
nSteps     = 20
similarityThreeshold = 0.4         # Agent is happy if at least 40% similar
neighborhood = 5                   # Defining how far looking for similar agents
mypal        = [:white,:red,:blue] # First colour is for the empty cell
gids         = [1,2]               # Gid 0 is reserved for empty cell
grShares     = [0.4,0.4]           # Shares of cells occupied by agents, by type


# ------------------------------------------------------------------------------
# ## 6) Initialising the simulation with the given parameters...
nCells = nR*nC
nGroups = length(gids)
grsizes= Int.(ceil.(nCells .* grShares))

cells  = fill(ShellingAgent(0),nCells)

global count = 1
for g in 1:nGroups
    [cells[j] = ShellingAgent(gids[g]) for j in count:count+grsizes[g]-1]
    global count += grsizes[g]
end 

shuffle!(cells)

save_cells_to_txt(cells, "cells.txt")

command_to_run1 = `ghc haskell_schelling.hs -package containers -o haskell_schelling`
run(command_to_run1)
command_to_run2 = `./haskell_schelling cells.txt cells_out.txt`
run(command_to_run2)

gids = readlines("cells_out.txt")
gids = parse.(Int, gids)
cells = ShellingAgent.(gids)



env = Env(nR,nC,similarityThreeshold,neighborhood,nSteps,cells,gids,grsizes)
outplot = heatmap(printableGrid(env), legend=nothing, title="Result", color=mypal,aspect_ratio=env.nR/env.nC, size=(600,600*env.nR/env.nC))
display(outplot)

println("Happy tortoises: ", happy_perc(env))