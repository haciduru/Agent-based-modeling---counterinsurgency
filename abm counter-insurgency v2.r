
rm(list = ls())
library(tictoc)

# Auxiliary functions
{
  # This function cleans the screen.
  cls = function() cat(rep('\n', 100))
  
  # This function randomly selects an item index from x. The probability of being
  # selected is correlated with the item value. 
  selectx = function(x) sum(runif(1) > cumsum(x / sum(x)))+1
  
  # This function transforms value to its sigmoid.
  sigmoid = function(x) return(1/(1+exp(-x)))
}

# Military functions
{
  # This function recruits a civilian agent to the military. It picks the the civilian
  # agent with the lowest support for the insurgency.
  recruit.mil = function() {
    civs = unlist(lapply(agents, function(a) if (class(a) == civ) a else NULL))
    scivs = unlist(lapply(civs, function(a) a$sup))
    a = civs[scivs == min(scivs)][[1]]
    class(a) = mil
    a$sup = .1
  }
}

# Network functions
{
  # This function checks whether two environments are wired to each other.
  wired = function(a,b)
    b$id %in% unlist(lapply(a$nei, function(b) b$id))
  
  # This function wires two environments to each other.
  wire = function(a,b) {
    if (a$id != b$id & !wired(a,b)) {
      a$nei = append(a$nei, b)
      b$nei = append(b$nei, a)
    }
  }
  
  # This function unwires two environments from each other.
  unwire = function(a,b) {
    a$nei = a$nei[unlist(lapply(a$nei, function(b) b$id)) != b$id]
    b$nei = b$nei[unlist(lapply(b$nei, function(a) a$id)) != a$id]
  }
}

# Base functions
{
  # This function creates a new agent.
  new_agent = function(id, class=civ) {
    
    # base
    a = new.env()
    a$id = id
    class(a) = class
    
    # social dna
    a$dna = numToBits(rnorm(1))
    
    # sociability
    a$tol = rpois(1,5)+1
    
    # friends
    a$nei = list()
    
    # location
    a$loc = sample(geo, 1)[[1]]
    a$loc$occ = append(a$loc$occ, a)
    
    # resource
    a$res = 0
    
    # path
    a$path = list(a$loc)
    
    # support
    a$sup = sample(skew, 1)
    
    # memory
    a$mem = create.int(a)
    
    return(a)
    
  }
  
  # This function creates a list of N agents.
  init.agents = function(N=100)
    lapply(c(1:N), function(x) new_agent(x))
}

# Geography functions
{
  library(igraph)

  # This function creates a new node.
  new_node = function(x,y,id,class='darkgray') {
    n = new.env()
    class(n) = 'darkgray'
    n$x = x
    n$y = y
    n$id = id
    return(n)
  }
  
  # This function creates a list of the edges of a grid network.
  create_grid = function(r,c) {
    mat = matrix(c(1:(r*c)), ncol = c)
    net = matrix(c(0,0), nrow = 1)
    i = 0
    while (i < (c-1)) {
      i = i + 1
      net = rbind(net, mat[,c(i,i+1)])
      mat; mat[,c(i,i+1)]
    }
    i = 0
    while (i < (r-1)) {
      i = i + 1
      net = rbind(net, t(mat[c(i,i+1), ]))
    }
    return(net[-1,])
  }
  
  # This function creates the geographic environment. 
  init.geo = function(X,Y,prm=.3) {
    net = create_grid(Y, X)
    g = graph_from_data_frame(as.data.frame(net))
    V(g)$id = c(1:(Y*X))
    g = g - sample(V(g), round(length(V(g))*(prm)))
    g = decompose(g)
    x = unlist(lapply(g, function(x) length(x)))
    g = g[x == max(x)][[1]]
    ids = as.numeric(V(g)$id)
    
    nodes = list()
    id = 0
    for (x in 1:X) {
      for (y in 1:Y) {
        id = id + 1
        nodes = append(nodes, new_node(x,y,id))
      }
    }
    
    for (i in 1:nrow(net)) {
      u = net[i,1]; v = net[i,2]
      wire(nodes[[net[i,1]]], nodes[[net[i,2]]])
    }
    
    for (i in 1:length(nodes)) {
      if (!(i %in% ids)) {
        a = nodes[[i]]
        lapply(a$nei, function(b) unwire(a,b))
      }
    }
    
    nodes = unlist(lapply(nodes, function(n) { if (length(n$nei) > 0) n }))
    
    return(nodes)
  }
}

# Suppport level functions
{
  # Random numbers to initiate support levels. s_norm has a normal distribution,
  # s_low has a skewed distribution (to the left) and s_high has a skewed distribution
  # to the right.
  {
    x = rnorm(1000)
    x = x - min(x)
    x = x / max(x)
    s_norm = x
    s_low = x ** 1.5
    s_high = x ** .5
  }  

  # This function increases an agent's support level.
  inc.sup = function(a, supstep=.1)
    a$sup = a$sup + (1 - a$sup)*supstep
  
  # This function decreases an agent's support level.
  dec.sup = function(a, supstep=.1)
    a$sup = a$sup - a$sup*supstep
  
  # This function returns the level of agreement in two agents' support levels 
  # for the insurgency.
  sup.fit = function(a,b) 1 - abs(a$sup - b$sup)
}

# Interaction functions
{
  # This function selects an agent to interact. It first creates a pool of possible
  # agents to interact. This pool is either the agents' local neighbors (i.e., agents
  # who are in the same geographic node or the adjacent nodes) or the agent's friends.
  selectb = function(a) {
    if (runif(1) < .5) {
      nei = unlist(lapply(append(a$loc, a$loc$nei), function(l) l$occ))
      nei = nei[unlist(lapply(nei, function(b) b$id)) != a$id]
    } else {
      nei = a$nei
    }
    if (length(nei)) {
      b = sample(nei, 1)[[1]]
    } else {
      b = NULL
    }
    return(b)
  }
  
  # This function makes two agents (a and b) interact. The agents first talk (i.e.,
  # a gives intel to b) and then if the social DNA fit between them is high, they
  # become friends.
  interact = function(a) {
    b = selectb(a)
    if (length(b)) {
      talk(a,b)
      if (dna.fit(a,b) > min.dna.fit.2.become.friends) {
        wire(a,b)
        if (length(a$nei) > a$tol) {
          x = unlist(lapply(a$nei, function(b) dna.fit(a,b) * sup.fit(a,b)))
          unwire(a, a$nei[x == min(x)][[1]])
        }
      }
    }
  }
  
  # This function creates intel. Agent a creates intel that they will give to agent
  # b. The intel incolves agent a's location and support level for the insurgency.
  # Agent a is not totally honest about their support level. It depends of the level
  # of support that agent b feels for the insurgency. If agent a does not know agent
  # b's support level, it assumes that it is .5.
  create.int = function(a,b=NULL) {
    
    recall = function(a,b) {
      a$mem[unlist(lapply(a$mem, function(i) b$id)) == b$id]
    }
    
    rsup = function(x,y) {
      ifelse(x>y, runif(1,min=y,max=x), runif(1,min=x,max=y))
    }
    
    int = recall(a,b)
    if (length(int) > 0) {
      int = list(id = a$id, loc = a$loc, sup = rsup(a$sup, int[[1]]$sup))
    } else {
      int = list(id = a$id, loc = a$loc, sup = rsup(a$sup, .5))
    }
    
    return(list(int))
  }
  
  # This function makes agent a and b talk; agent a gives intel to agent b.
  talk = function(a,b) {
    if (runif(1) < .5) {
      int = create.int(a,b)
    } else {
      int = a$mem[selectx(1/log(c(2:(length(a$mem)+1))))]
    }
    if (int[[1]]$id != b$id) {
      b$mem = b$mem[unlist(lapply(b$mem, function(a) a$id)) != int[[1]]$id]
      b$mem = append(int, b$mem)
    }
    if (length(b$mem) > mem_len) b$mem = b$mem[1:mem_len]
  }
}

# Display functions
{
  report = function(a) {
    als = ls(a)
    als = als[als != 'loc']
    cat('\nclass:\t', class(a), sep = '')
    for (i in als) {
      item = get(i, envir=a)
      if (typeof(item) != 'list') {
        cat('\n', i, ' (', typeof(item),')', ':\t', item, sep = '')
      } else {
        cat('\n', i, ' (', typeof(item),')', ':\t', length(item), sep = '')
      }
    }
    cat('\nloc (env):\t', a$loc$id, sep = '')
    cat('\n')
  }
  
  display = function() {
    agents = unlist(lapply(agents, function(a) if (length(a$nei) > 0) a else NULL))
    if (length(agents)) {
      g = graph_from_data_frame(
        as.data.frame(
          t(matrix(unlist(lapply(agents, function(a) { lapply(a$nei, function(b) c(a$id, b$id)) })),nrow = 2))))
      g = as.undirected(g)
      vs = unlist(lapply(agents, function(a) log(length(a$nei)+1)))
      vc = unlist(lapply(agents, function(a) class(a)))
      plot(g, vertex.label='', vertex.size=vs, vertex.color=vc,
           vertex.frame.color=vc, edge.width=.5, edge.color = 'gainsboro',
           layout=layout_nicely)
    } else {
      cat('There is nothing to display!\n')
    }
  }
  
  geo.display = function() {
    par(oma=c(0,0,0,0))
    par(mar=c(0,0,0,0))
    locs = t(matrix(unlist(lapply(agents, function(a) {
      x = a$loc$x
      y = a$loc$y
      x = runif(1, x - .4, x + .4)
      y = runif(1, y - .4, y + .4)
      return(c(x,y))
    })), nrow = 2))
    cols = unlist(lapply(agents, function(a) class(a)))
    plot(locs, pch = 16, col = cols, cex = .5, xlab = '', ylab = '', axes = F)
  }
}

# social dna
{
  dna.fit = function(a,b) mean(a$dna == b$dna)
  
  swapdna = function(a,b) {
    bit2swap = sample(c(1:length(a$dna)), 1)
    x = selectx(c(length(a$nei)+1,
                  length(b$nei)+1))
    if (x == 1) {
      b$dna[bit2swap] = a$dna[bit2swap]
    } else {
      a$dna[bit2swap] = b$dna[bit2swap]
    }
  }
}

# move
{
  move = function(a) {
    d = sample(a$loc$nei, 1)[[1]]
    a$loc$occ = a$loc$occ[unlist(lapply(a$loc$occ, function(a) a$id)) != a$id]
    a$loc = d
    d$occ = append(a$loc$occ, a)
  }
}

# combat
{
  find.target = function(a) {
    b = NULL
    targets = a$mem[unlist(lapply(a$mem, function(b) b$id)) != a$id]
    if (length(targets) > 0) {
      sups = unlist(lapply(targets, function(b) b$sup))
      if (class(a) == mil) {
        target = targets[sups == max(sups)][[1]]  
      } else {
        target = targets[sups == min(sups)][[1]]
      }
      if (abs(a$sup-target$sup) > attackt) {
        a$mem = a$mem[unlist(lapply(a$mem, function(b) b$id)) != target$id]
        target = unlist(lapply(agents, function(a) if (a$id == target$id) a))
        if (length(target) > 0) b = target[[1]]
      }
    }
    return(b)
  }
  
  attack = function(a,b) {
    
    if (class(a) == mil) {
      
      # military attack
      cry(b)
      if (class(b) == ins) {
        n_ins_captur <<- n_ins_captur + 1
        die(b)
      } else {
        n_col_damage <<- n_col_damage + 1
      }
    
    } else {
      
      # insurgent attack
      n_ins_attack <<- n_ins_attack + 1
      
      # target is civilian
      if (class(b) == civ) {
        
        if (runif(1) < .05) {
          n_ins_d_atta <<- n_ins_d_atta + 1
          cry(b); die(b)
        } else {
          b$sup = .5
        }
        
      } else {
        
        # target military
        if (class(b) == mil) {
          if (runif(1) < .5) {
            die(a)
          } else {
            n_ins_d_atta <<- n_ins_d_atta + 1
            cry(b); die(b)
          }
        }
        
        #target is social worker
        if (class(b) == soc) {
          cry(b); die(b)
        }
        
      }
    }
  }
    
  die = function(a) {
    invisible(lapply(a$nei, function(b) unwire(a,b)))
    a$loc$occ = a$loc$occ[unlist(lapply(a$loc$occ, function(b) b$id)) != a$id]
    agents <<- agents[unlist(lapply(agents, function(a) a$id)) != a$id]
    if (class(a) == soc) recruit.soc()
    if (class(a) == mil) recruit.mil()
  }
  
  cry = function(a) {
    
    if (a$sup > .5) {
      inc.sup(a, .2)
      invisible(lapply(a$nei, function(b) inc.sup(b)))
      invisible(lapply(a$loc$occ, function(b) inc.sup(b, .05)))
    } else {
      dec.sup(a, .2)
      invisible(lapply(a$nei, function(b) dec.sup(b)))
      invisible(lapply(a$loc$occ, function(b) dec.sup(b, .05)))
    }
    
  }
}

# resource
{
  share = function(a) {
    nei = append(
      lapply(a$nei, function(b) list(x = 1, a = b)),
      lapply(a$loc$occ, function(b) list(x = .2, a = b))
    )
    nei = nei[unlist(lapply(nei, function(b) b$a$res)) < a$res]
    if (length(nei)) {
      b = nei[[selectx(unlist(lapply(nei, function(b) b$x)))]]$a
      b$res = b$res + a$res / 2
    }
    if (class(a) != soc) a$res = 0
  }
  
  recruit.soc = function() {
    civs = unlist(lapply(agents, function(a) if (class(a) == civ) a else NULL))
    scivs = unlist(lapply(civs, function(a) a$sup))
    a = civs[scivs == min(scivs)][[1]]
    class(a) = soc
    a$res = 1
    a$sup = .1
  }
  
  swork = function(a) {
    cl = a$loc$occ[unlist(lapply(a$loc$occ, function(b) b$sup)) > sworkt]
    if (length(cl)) {
      b = sample(cl, 1)[[1]]
      b$res = .001
    } else {
      move(a)
    }
  }
}

# main
{
  init = function() {
    geo <<- init.geo(gX,gY)
    
    agents <<- init.agents(n_agents); a = agents[[1]]; b = agents[[2]]
    
    pool = unlist(lapply(agents, function(a) if (class(a) == civ) a))
    invisible(lapply(sample(pool, ceiling(length(agents) * psoc)), function(a) {
      class(a) = soc
      a$res = 1
      a$sup = supmin
    }))
    
    pool = unlist(lapply(agents, function(a) if (class(a) == civ) a))
    invisible(lapply(sample(pool, ceiling(length(agents) * pmil)), function(a) { 
      class(a) = mil
      a$sup = supmin
    }))
    
    pool = unlist(lapply(agents, function(a) if (class(a) == civ) a))
    invisible(lapply(sample(pool, ceiling(length(agents) * pins)), function(a) {
      class(a) = ins
      a$sup = supmax
    }))
  }
  
  iterate = function() {
    invisible(lapply(agents, function(a) {
      
      if (runif(1) < .05) move(a)
      
      interact(a)
      
      if (class(a) == civ) {
        if (a$res > 0) dec.sup(a, a$res)
        share(a)
      }
      
      if (class(a) == soc) swork(a)
      
      if (class(a) %in% c(mil, ins)) {
        b = find.target(a)
        if (length(b) > 0) attack(a,b)
      }

    }))
  }
  
  run = function(skew, psoc, pmil) {
    
    skew <<- skew
    psoc <<- psoc
    pmil <<- pmil
    
    n_ins_attack <<- 0
    n_ins_captur <<- 0
    n_ins_d_atta <<- 0
    n_col_damage <<- 0
    
    init()
    
    n_civ1 = sum(unlist(lapply(agents, function(a) if (class(a) == civ) 1 else 0)))
    n_soc1 = sum(unlist(lapply(agents, function(a) if (class(a) == soc) 1 else 0)))
    n_mil1 = sum(unlist(lapply(agents, function(a) if (class(a) == mil) 1 else 0)))
    n_ins1 = sum(unlist(lapply(agents, function(a) if (class(a) == ins) 1 else 0)))
    
    pre_sup = unlist(lapply(agents, function(a) if (class(a) == civ) a$sup else NULL))
    
    nins = sum(unlist(lapply(agents, function(a) if (class(a) == ins) 1)))
    niter = 0
    while (niter < 10 & nins > 0) {
      niter = niter + 1
      tic(niter)
      for (i in 1:365) iterate()
      toc()
      cat('\nn_ins_attack:', n_ins_attack,
          '\nn_ins_d_atta:', n_ins_d_atta,
          '\nn_col_damage', n_col_damage,
          '\nn_ins_captur', n_ins_captur, '\n')
      print(table(unlist(lapply(agents, function(a) class(a)))))
      cat('\n')
      nins = sum(unlist(lapply(agents, function(a) if (class(a) == ins) 1)))
      toc()
    }
    
    n_civ2 = sum(unlist(lapply(agents, function(a) if (class(a) == civ) 1 else 0)))
    n_soc2 = sum(unlist(lapply(agents, function(a) if (class(a) == soc) 1 else 0)))
    n_mil2 = sum(unlist(lapply(agents, function(a) if (class(a) == mil) 1 else 0)))
    n_ins2 = sum(unlist(lapply(agents, function(a) if (class(a) == ins) 1 else 0)))
    
    post_sup = unlist(lapply(agents, function(a) if (class(a) == civ) a$sup else NULL))
    
    df_sup = rbind(data.frame(sup = pre_sup, tx = 0), data.frame(sup = post_sup, tx = 1))
    model = summary(lm(sup ~ tx, data = df_sup))
    
    res = data.frame(
      n_agents = n_agents,
      pmil = pmil,
      pins = pins,
      psoc = psoc,
      coef_intercept = model$coefficients[1,1],
      coef_tx = model$coefficients[2,1],
      se_intercept = model$coefficients[1,2],
      se_tx = model$coefficients[2,2],
      t_intercept = model$coefficients[1,3],
      t_tx = model$coefficients[2,3],
      n_ins_attack = n_ins_attack,
      n_ins_captur = n_ins_captur,
      n_ins_d_atta = n_ins_d_atta,
      n_col_damage = n_col_damage,
      niter = niter,
      n_civ1 = n_civ1,
      n_soc1 = n_soc1,
      n_mil1 = n_mil1,
      n_ins1 = n_ins1,
      n_civ2 = n_civ2,
      n_soc2 = n_soc2,
      n_mil2 = n_mil2,
      n_ins2 = n_ins2
    )
    
    return(res)
  }
}

# params
{
  n_agents = 25000
  mem_len = 40
  skew = s_norm
  supstep = .1

  civ = 'darkgray'
  soc = 'purple'
  ins = 'firebrick'
  mil = 'dodgerblue'
  
  sworkt = .5
  attackt = .7
  
  supmax = .9
  supmin = .1
  
  min.dna.fit.2.become.friends = .7
  
  psoc = .003
  pmil = .003
  pins = .0008
  
  gX = 30
  gY = 30
}

save_res = function() {
  dt = paste(Sys.time())
  dt = gsub(':', '.', dt)
  write.csv(RES, paste('results', dt, '.csv', sep = ''), row.names = F)
}

# Global: these are global registries that functions store data temporarily.
{
  n_ins_attack = 0
  n_ins_captur = 0
  n_ins_d_atta = 0
  n_col_damage = 0
}

# RES
{
  RES = data.frame(
    seed = NA,
    n_agents = NA,
    pmil = NA,
    pins = NA,
    psoc = NA,
    coef_intercept = NA,
    coef_tx = NA,
    se_intercept = NA,
    se_tx = NA,
    t_intercept = NA,
    t_tx = NA,
    n_ins_attack = NA,
    n_ins_captur = NA,
    n_ins_d_atta = NA,
    n_col_damage = NA,
    niter = NA,
    n_civ1 = NA,
    n_soc1 = NA,
    n_mil1 = NA,
    n_ins1 = NA,
    n_civ2 = NA,
    n_soc2 = NA,
    n_mil2 = NA,
    n_ins2 = NA
  )  
}


while (T) {
  
  # base support
  seed = as.numeric(Sys.time())
  set.seed(seed)
  cls()
  res = run(s_norm, .003, .003)
  res = cbind(data.frame(seed = seed), res)
  RES = rbind(RES, res)
  save_res()
  
  seed = as.numeric(Sys.time())
  set.seed(seed)
  cls()
  res = run(s_norm, .003, .008)
  res = cbind(data.frame(seed = seed), res)
  RES = rbind(RES, res)
  save_res()

  seed = as.numeric(Sys.time())
  set.seed(seed)
  cls()
  res = run(s_norm, .008, .003)
  res = cbind(data.frame(seed = seed), res)
  RES = rbind(RES, res)
  save_res()
  
  # high support
  seed = as.numeric(Sys.time())
  set.seed(seed)
  cls()
  res = run(s_high, .003, .003)
  res = cbind(data.frame(seed = seed), res)
  RES = rbind(RES, res)
  save_res()
  
  seed = as.numeric(Sys.time())
  set.seed(seed)
  cls()
  res = run(s_high, .003, .008)
  res = cbind(data.frame(seed = seed), res)
  RES = rbind(RES, res)
  save_res()
  
  seed = as.numeric(Sys.time())
  set.seed(seed)
  cls()
  res = run(s_high, .008, .003)
  res = cbind(data.frame(seed = seed), res)
  RES = rbind(RES, res)
  save_res()
  
  # low suppport
  seed = as.numeric(Sys.time())
  set.seed(seed)
  cls()
  res = run(s_low, .003, .003)
  res = cbind(data.frame(seed = seed), res)
  RES = rbind(RES, res)
  save_res()
  
  seed = as.numeric(Sys.time())
  set.seed(seed)
  cls()
  res = run(s_low, .003, .008)
  res = cbind(data.frame(seed = seed), res)
  RES = rbind(RES, res)
  save_res()
  
  seed = as.numeric(Sys.time())
  set.seed(seed)
  cls()
  res = run(s_low, .008, .003)
  res = cbind(data.frame(seed = seed), res)
  RES = rbind(RES, res)
  save_res()
  
}
