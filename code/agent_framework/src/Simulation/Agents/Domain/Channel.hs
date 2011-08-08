package Simulation.Agents.Domain.Channel where

  --Wider question - is pubsub enough to model all the various types of agent topology, including models that have agents interacting in cartesian space?
  -- Look into this - My gut feeling says yes if agents can subscribe and unsubscribe to channels during the course of the simulation. Find a source for a list of common topologies and attempt to show this for a selection. Eg.Game of Life? Von-Neumann  5-neighbour /Moore 9-neighbour / GIS / Euclidian  / Network

  class Channel a --Not sure about this.
