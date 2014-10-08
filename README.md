cl-plot
=======

A plotting library in Common Lisp, based on [Cairo](http://cairographics.org) and [cl-cairo2](http://github.com/rpav/cl-cairo2).

## What is it?

Somehow, Lisp lacks a good plotting solution. Most Lisp plotting packages are either interfaces to to gnuplot or bindings to C libraries such as pl-plot. What's missing is a Lisp-y plotting interface that does for CL what Matplotlib does for Python. All the pieces that make up a plot (figure, axes, data, etc.) should be CLOS objects that the user can manipulate if they wish. The only outside dependency is cl-cairo2 (and Cairo itself, obviously). Cairo is great for producing vector graphics and cl-cairo2 is very stable.

## Features

cl-plot is currently quite barebones. It implements a figure canvas, sets of axes, and data containers. The 2D plot function currently produces a set of axes, transforms the data into axes coordinates, and plots the data as points. At the moment, only plotting to a file is supported; plotting to a window requires adding a dependency on Gtk or some other GUI toolkit, which is more complexity than I want to get into at the moment.

## Installation

Someday this will all be in [Quicklisp](http://quicklisp.org). For now, do

	git clone http://github.com/grapesmoker/cl-plot.git
	ln -s /path/to/cl-plot ~/quicklisp/local-projects/cl-plot
	
and then in your favorite REPL

	(ql:quickload :cl-plot)
	
This assumes you have correctly set up quicklisp. 

## Usage

Currently, you can't do a whole lot other than plot 2D data (without labels or tick marks). To verify that things work, you can do

	(cl-plot:draw-points-test "/path/to/output.png" 100)
	
This will produce a PNG with 100 randomly sized and randomly colored points. To plot some data, try:

	(let* ((x (loop for i from 0 to 100 collect (* i (/ (* 2 pi) 100)) (y (mapcar #'sin x)))
		(plot x y "/path/to/output.png"))
		
asda
	
