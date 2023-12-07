library(deSolve)

sir <- function(time, vars, parms){
	S <- with(as.list(vars), exp(lS))
	I <- with(as.list(vars), exp(lI))
	R <- with(as.list(vars), exp(lR))

	return(with(parms, list(c(
		lSdot = μ*N/S - β*I/N - μ
		, lIdot = β*S/N-γ-μ
		, lRdot = γ*I/R - μ
	))))
}

sim <- function(Sinit=NULL, Iinit=1, Rinit=1, R0=5, N=10000
	, D=1, ρ=1e-3, finTime=100, timeStep=0.1
	, dfun=sir
){
	if(is.null(Sinit)){Sinit <- N-Iinit-Rinit}
	sim <- as.data.frame(ode(
		y=c(lS=log(Sinit), lI=log(Iinit), lR=0)
		, func=dfun
		, times=seq(from=0, to=finTime, by=timeStep)
		, parms=list(γ=1/D, β=R0/D, μ=ρ/D, N=N)
	))

	return(within(sim, {
		S <- exp(lS)
		I <- exp(lI)
		R <- exp(lR)
		Ncalc <- S+I+R
	}))
}

print(sim())

quit()
print(sim(ρ=0))
