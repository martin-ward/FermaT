%!PS-Adobe-2.0
%%Pages: Atend
%%PageOrder: ascend
%%BoundingBox: 0 0 596 842
%%EndComments
%%BeginProcSet: callgr.pro
%
% Call graph prologue file
%
% ============================================================================
% FermaT Transformation System
% Copyright (C) 2001 Software Migrations Limited.
% Email: martin@gkc.org.uk
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 3 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.
% ==============================================================================
%

% First define some parameters:

/arc_angle 8 def
/text_size 10 def
/arrow_dim 2 def
/arrow_size 3 def
/arrow_length 1.4 def
/arrow_inset 0.4 def
/unit_length 5 def
/frame_sep 2.5 def
/frame_arc 5 def
/line_arc 5 def

% x coordinate of the ??? box for ASSEMRET
/qqq_indent 10 def

% If label is a non-empty string, then use it to label the line/arc etc:
/label () def

1 setlinecap % line cap style (0=butt, 1=round, 2=projecting)

% These are computed from arrow_length:
/ar_width 4.4 def
/ar_length 6.16 def
/ar_inset 2.464 def

/ED { exch def } bind def


% Set the text size and calculate text height and depth:
% stack: font size  -->
/set_text_size {
  dup /text_size ED
  exch
  findfont exch scalefont setfont
  % compute the bounding box for "(" to get the height and depth of a "strut":
  newpath 0 0 moveto
  (\() true charpath flattenpath pathbbox
  % stack contains llx lly urx ury,
  % where lly is text depth and ury is text height
  /text_height ED pop neg /text_depth ED pop
  % Use "0" to get the height and depth of a label:
  newpath 0 0 moveto
  (0) true charpath flattenpath pathbbox
  /label_height ED pop neg /label_depth ED pop
  % compute the bounding box for "x" to get unit_length (1ex)
  % from the height of "x" (ie ury)
  newpath 0 0 moveto
  (x) true charpath flattenpath pathbbox	% stack: llx lly urx ury
  % top of stack is the required unit_length
  
  dup 2 div /frame_sep ED	% frame_sep is unit_length/2  
  dup 5 div setlinewidth	% line_width is unit_length/5  
  dup /line_arc ED		% line_arc is unit_length
  frame_arc 0 ne {
    dup /frame_arc ED		% frame_arc is unit_length, if non-zero
  } if
  5 /arrow_size ED		% arrow_size

  /unit_length ED pop pop pop	% stack is empty
  % text_vadjust is the distance between the baseline and the midline:
  % /text_vadjust = (text_height - text_depth)/2
  text_height text_depth sub 2 div /text_vadjust ED

  % Compute other arrow dimensions from arrow_size:
  /ar_width arrow_size currentlinewidth mul arrow_dim add def
  /ar_length arrow_length ar_width mul def
  /ar_inset arrow_inset ar_length mul def

  % Re-compute qqq_indent for the new text size:
  (???) stringwidth pop 2 div frame_sep add currentlinewidth add
  25 add /qqq_indent ED % Printing starts about 25 pts from the left edge

} def


% plot a framed box around the given text at the given position
% If frame_arc > 0 then the corners are rounded with that radius
% stack: (text) x y  -->
% where (x,y) is to be the midpoint of the box
/fbox {
  plot_text		% stack: frame_width frame_height
  % currentpoint is now bottom left corner of box
  draw_frame
  stroke
} def


% plot contents of label (if any) at given posn:
% stack: x y  -->
/plot_label {
  0 begin
    label () eq {
      pop pop
    } {
      /y ED /x ED
      label stringwidth pop		% stack: width
      frame_sep 2 mul add /w ED		% stack empty
      label_height label_depth add
      frame_sep 2 mul add /h ED
      % plot a white box, then plot the label:
      currentrgbcolor			% stack: r g b
      1 setgray
      newpath
      x w 2 div sub y h 2 div sub moveto % currentpoint = ll
      0 h rlineto
      w 0 rlineto
      0 h neg rlineto
      closepath
      fill
      setrgbcolor			% stack empty
      label x y text_depth label_depth add 2 div sub
      plot_text pop pop		% throw away the returned width/height
    } ifelse
  end
} def

/plot_label load 0 4 dict put	% replace 0 by an anon dict
    

% plot the text centered on the given position
% and prepare for drawing a rectangular frame or rounded frame
% stack: (text) x y  -->  frame_width frame_height
% Current posn is moved to (llx, lly) of text bounding box
/plot_text{
  newpath
  moveto					% stack: (text)
  % calculate the width of the text, then move right by width/2
  % and down by text_vadjust, print the text without changing the position.
  dup stringwidth pop 				% stack: (text) width
  dup 2 div neg text_vadjust neg rmoveto 	% stack: (text) width
  exch gsave show grestore			% stack: width
  % current point is now at the start of the text
  0 text_depth neg rmoveto		% move to llx lly of text bounding box
  frame_sep neg currentlinewidth 2 div sub	% calculate drawing adjustment
  dup rmoveto				% move to bottom left corner of frame
  % Now calculate width and height of frame for drawing, stack = width
  frame_sep 2 mul add
  currentlinewidth add			% stack: frame_width
  text_depth text_height add
  frame_sep 2 mul add
  currentlinewidth add			% stack: frame_width frame_height
  % current point is now at llx lly of required box
} def


% plot a box with bottom left corner at current position,
% stack: width height  --> 
/draw_frame {
  0 begin
    frame_arc 0 eq {
      2 copy                % stack: width height width height
      0 exch rlineto        % stack: width height width
      0 rlineto             % stack: width height
      neg 0 exch rlineto    % stack: width
      pop
      closepath
    } {
      /w ED /h ED % stack is empty
      % move the origin to corner of box and keep on stack:
      currentpoint 2 copy translate
      % move up to start of first vertical segment
      0 frame_arc moveto
      0 w h w frame_arc arcto pop4
      h w h 0 frame_arc arcto pop4
      h 0 0 0 frame_arc arcto pop4
      0 0 0 w frame_arc arcto pop4
      closepath
      % restore the original origin:
      neg exch neg exch translate
    } ifelse
  end
} def

/draw_frame load 0 2 dict put	% replace 0 by an anon dict

/pop4 {
  pop pop pop pop
} def


% Calculate the size of a box around the given text, including the line width
% stack: (text)  -->  frame_width frame_height
/frame_size {
  stringwidth pop 			% stack: width
  % calculate the amount to add to both width and height
  frame_sep currentlinewidth add 2 mul	% stack: width adjustment
  add 					% stack: frame_width
  text_depth text_height add		% stack: frame_width height
  frame_sep currentlinewidth add 2 mul	% stack: frame_width height adjustment
  add					% stack: frame_width frame_height
} def


% Find the intersection point of a line from (0,0) at a given angle
% with the frame of a given (external) size, centered at (0,0).
% (ie the result is the adjustment to add to the line endpoint
% so that it just reaches the frame).
% stack: width height theta  -->  dx dy
/frame_line_intersect {
  0 begin
    mod360 /theta ED		% stack: width height
    2 div /y ED 2 div /x ED	% stack empty
    /xs 1 def
    /ys 1 def
    /phi y x atan def			% phi = angle of line to (urx, ury)
    % determine which quadrant the line is in:
    theta 180 ge {		% IF theta >= 180 THEN
      /ys ys neg def		%   flip about y axis, ie invert dy
      /theta 360 theta sub def	%   and subtract theta from 360 
    } if
    theta 90 ge {		% IF theta >= 90 THEN
      /xs xs neg def		%   flip about x axis, ie invert x
      /theta 180 theta sub def	%   and subtract theta from 180
    } if
    phi theta le {		% IF phi <= theta THEN
      y theta cos theta sin div mul xs mul
      y ys mul
    } {
      x xs mul
      x theta sin theta cos div mul ys mul
    } ifelse
  end
} def

/frame_line_intersect load 0 6 dict put	% replace 0 by an anon dict

% Adjust top of stack to range 0..360 (for frame_line_intersect)
% stack: theta  -->  theta mod 360
/mod360 {
  { 			% DO
    dup 360 gt {	%   IF theta > 360 THEN
      360 sub		%     theta := theta - 360
    } {			%   ELSE
      dup 0 lt {	%     IF theta < 0 THEN
	360 add		%	theta := theta + 360
      } {		%     ELSE
	exit		%       EXIT
      } ifelse		%     FI
    } ifelse		%    FI
  } loop		% OD
} def


% Calculate the start/end coordinates of a line from:
% (foo) x1 y1
% to:
% (bar) x2 y2
% stack: str1 x1 y1 str2 x2 y2  -->  X1 Y1 X2 Y2
% NB: If either string is empty, then the corresponding coordinates
% are not adjusted.
/calculate_line_coords {
  0 begin
    /y2 ED /x2 ED /str2 ED
    /y1 ED /x1 ED /str1 ED
    % First calculate the angle theta: (x1, y1) -> (x2, y2)
    /theta y2 y1 sub x2 x1 sub atan def
    % Check for an empty str1:
    str1 () ne {
      % Calculate dx1 dy1 from the frame size for str1:
      str1 frame_size		% stack: width1 height1
      theta
      frame_line_intersect	% stack: dx1 dy1
      % adjust x1 and y1:
      y1 add /y1 ED		% stack: dx1
      x1 add /x1 ED		% stack empty
    } if
    str2 () ne {
      % For x2 y2, the angle direction must be reversed (add 180 mod 360)
      str2 frame_size		% stack: width2 height2
      theta 180 add		% stack: reversed_theta
      frame_line_intersect	% stack: dx2 dy2
      % adjust x2 and y2:
      y2 add /y2 ED		% stack: dx2
      x2 add /x2 ED		% stack empty
    } if

    % push the result onto the stack:
    x1 y1 x2 y2
  end
} def

/calculate_line_coords load 0 7 dict put


% draw a line from x1 y1 to x2 y2
/line {
  0 begin
    newpath
    /y2 ED /x2 ED
    moveto
    x2 y2 lineto
    stroke
  end
} def

/line load 0 2 dict put


% draw a line with an arrow head from x1 y1 to x2 y2
% The basic method: (1) calculate theta, and line length,
% (3) translate to x2 y2, (4) rotate coordinates by theta,
% (4) draw the line: (-length, 0) to (-(ar_length - ar_inset), 0)
% (5) draw an arrow head pointing right to (0,0)
% (6) add a label if required
% stack: x1 y1 x2 y2  -->
/arrow {
  0 begin
    /y2 ED /x2 ED
    /y1 ED /x1 ED		% stack empty
    x2 x1 sub dup mul		% stack: dx^2
    y2 y1 sub dup mul		% stack: dx^2 dy^2
    add sqrt /line_length ED	% stack empty
    /theta y2 y1 sub x2 x1 sub atan def
    % Store the current transformation matrix:
    /savematrix matrix currentmatrix def
      x2 y2 translate
      theta rotate
      newpath
      line_length neg 0 moveto
      ar_length ar_inset sub neg 0 lineto
      stroke
      arrow_head
    savematrix setmatrix
    x1 x2 add 2 div y1 y2 add 2 div plot_label
  end
} def

/arrow load 0 7 dict put

% draw an arrow head pointing right to coordinate (0,0): 
/arrow_head {
  0 0 moveto
  ar_length neg ar_width 2 div lineto
  ar_length neg ar_inset add 0 lineto
  ar_length neg ar_width 2 div neg lineto
  closepath fill
} def


% A "recursion arrow" on a frame -- it comes out of the right
% edge, bends around the corner and goes in at the top
% with an arrowhead.
% The label is half way up the vertical segment (x2, y1) -> (x2 y2)
% stack: str x y  -->
/rec {
  0 begin
    /y ED /x ED			% stack: str
    frame_size 2 copy		% stack: width height width height
    % Starting angle is 0:
    0 frame_line_intersect	% stack: width height dx dy
    % Calculate the start point:
    y add /y1 ED		% stack: width height dx
    x add /x1 ED		% stack: width height
    % Ending angle is 90:
    90 frame_line_intersect	% stack: dx dy
    % Calculate the end point:
    y add /y3 ED
    x add /x3 ED		% stack empty
    % Calculate the coordinates of the upper right control point:
    % The arms are unit_length, so add line_arc to get the control point:
    x1 unit_length add line_arc add /x2 ED
    y3 unit_length add line_arc add /y2 ED
    newpath
    x1 y1 moveto
    x2 y1 x2 y3 line_arc arcto pop4
    x2 y3 x2 y2 line_arc arcto pop4
    x2 y2 x3 y2 line_arc arcto pop4
    x3 y2 x3 y3 line_arc arcto pop4
    currentpoint		% stack: x y (for start of arrow)
    stroke
    % Draw the final arrow (but don't label it!):
    x3 y3 unlabelled_arrow	% stack empty
    x2 y1 y2 add 2 div plot_label
  end
} def

/rec load 0 9 dict put


/unlabelled_arrow {
  0 begin
    /label () def
    arrow
  end
} def

/unlabelled_arrow load 0 1 dict put


% draw a curved line with an arrow head from str1 x1 y1 to str2 x2 y2
% The basic method: (1) Calculate the control point of the curve: x3 y3
% (2) Treat as two separate lines, 1 -> 3 and 3 -> 2
% (3) Adjust the end points x1 y1 and x2 y2
% (4) Adjust x2 y2 to shorten the curve by (ar_length - ar_inset)
%     to give x4 y4
% (5) Use curveto to draw the curve: x1 y1 x3 y3 x3 y3 x4 y4
% (6) Translate to x2 y2, rotate by (theta - arc_angle) and draw arrow_head
% (7) Add a label if needed, midway between the control points 3 & 4
%
% stack: str1 x1 y1 str2 x2 y2
/curve {
  0 begin
    /y2 ED /x2 ED /str2 ED
    /y1 ED /x1 ED /str1 ED	% stack empty
    x2 x1 sub dup mul		% stack: dx^2
    y2 y1 sub dup mul		% stack: dx^2 dy^2
    add sqrt /line_length ED	% stack empty
    /theta y2 y1 sub x2 x1 sub atan def

    % Calculate the angles of the two lines:
    /theta1 theta arc_angle add def
    /theta2 180 theta add arc_angle sub def

    % Calculate x3 y3:
    line_length 3 div dup dup dup
    theta1 sin mul y1 add /y3 ED
    theta1 cos mul x1 add /x3 ED % stack: l/3 l/3

    % Calculate x4 y4:
    theta2 sin mul y2 add /y4 ED
    theta2 cos mul x2 add /x4 ED % stack empty

    % Adjust the end points using theta1 and theta2:
    str1 frame_size
    theta1 frame_line_intersect	% stack dx1 dy1
    y1 add /y1 ED
    x1 add /x1 ED		% stack empty
    str2 frame_size
    theta2 frame_line_intersect	% stack dx2 dy2
    y2 add /y2 ED
    x2 add /x2 ED		% stack empty
    
    % We now have all four control points
    % Adjust x2 y2 in direction theta2 by (ar_length - ar_inset)
    % (to make space for the arrow head):
    ar_length ar_inset sub dup
    theta2 sin mul y2 add /y5 ED
    theta2 cos mul x2 add /x5 ED

    % Draw the curve (at last!)
    x1 y1 moveto x3 y3 x4 y4 x5 y5 curveto
    stroke

    % translate, rotate and draw the arrow head:
    % Store the current transformation matrix:
    /savematrix matrix currentmatrix def
      x2 y2 translate
      theta2 180 add rotate
      newpath
      arrow_head
    savematrix setmatrix

    % Label is midway between points 3 and 4:
    x3 x4 add 2 div y3 y4 add 2 div plot_label
  end 
} def

/curve load 0 17 dict put


% Draw a line with the arrow head in the middle:
% stack: x1 y1 x2 y2
/midarrow {
   0 begin
    /y2 ED /x2 ED
    /y1 ED /x1 ED	% stack empty
    % compute x3 y3 to be 2/3 along the line:
    /x3 x1 x2 2 mul add 3 div def
    /y3 y1 y2 2 mul add 3 div def
    x1 y1 x2 y2 line
    x1 y2 x3 y3 unlabelled_arrow
    x1 x2 add 2 div y1 y2 add 2 div plot_label
  end
} def

/midarrow load 0 6 dict put


% Draw a U-shaped line: down, along, up
% with given amount of space created (in unit_length units)
% Add an arrow head two-thirds along the straight bit,
% with a label one third along.
% NB there is no need to call frame_line_intersect, since the offset
% for an angle of 270 degrees is text_vadjust + frame_sep + linewidth
% stack: str1 x1 y1 str2 x2 y2 indent
/u_shape {
  0 begin
    /indent ED
    /y2 ED /x2 ED pop
    /y1 ED /x1 ED pop		% stack empty
    % calculate y adjustment:
    text_vadjust text_depth add frame_sep add currentlinewidth add dup
    y1 exch sub /y1 ED		% stack: y-adjustment
    y2 exch sub /y2 ED		% stack empty
    % Compute the control point's y-coordinate: y3
    y1 y2 min indent unit_length mul sub line_arc sub /y3 ED
    % draw the shape
    x1 y1 moveto
    x1 y3 x2 y3 line_arc arcto pop4
    x2 y3 x2 y2 line_arc arcto pop4
    x2 y2 lineto
    stroke
    % draw the arrow plus label:
    x1 line_arc add y3
    x1 x2 2 mul add 3 div y3 arrow
  end
} def

/u_shape load 0 6 dict put


% Compute the midpoint of a line segment:
% stack: x1 y1 x2 y2  -->  xm ym
/midpoint {
   0 begin
    /y2 ED /x2 ED
    /y1 ED /x1 ED	% stack empty
    % compute the midpoint:
    x1 x2 add 2 div
    y1 y2 add 2 div
  end
} def

/midpoint load 0 4 dict put


% atan takes a num and den and returns an angle,
% tan returns sin(a) cos(a):
/tan {
  dup sin exch cos
} def


% A useful test routine to find out where you are:
% draw crosshairs at the current point
/cross {
  gsave
    0 setlinewidth
    0 20 rmoveto
    0 -40 rlineto
    -20 20 rmoveto
    40 0 rlineto
    -20 0 rmoveto
    currentpoint 10 0 360 arc
    stroke
  grestore
} def

% Return the min of two numbers:
% a b  -->  min(a,b)
/min {
  2 copy lt {
    pop
  } {
    exch pop
  } ifelse
} def



% plot the text at currentpoint, draw a box around it, record
% the coords of the lower middle point in itemB
% the coord of the right middle point in itemR
% stack: str -->
/ANNOTATE {
  0 begin
    currentpoint               % stack: str x1 y1
    /y1 ED /x1 ED
    dup stringwidth pop        % stack: str width
    x1 add /x2 ED              % stack: str
    % Compute the midpoint of the box:
    x1 x2 add 2 div            % stack: str xm
    y1 text_vadjust add        % stack: str xm ym
    fbox                       % stack empty
    % calculate and record the nodes:
    node_dict begin
      /itemR [ x2 frame_sep add
	       y1 text_vadjust add ] def
      /itemB [ x1 x2 add 2 div
	       y1 text_depth sub frame_sep sub ] def
    end
    x2 y1 moveto
  end
} def

/ANNOTATE load 0 4 dict put

/HIGHLIGHT { ANNOTATE } def


% move to beginning of line, save the node and plot the text:
% stack: ypos node str
/ASSEM {
  0 begin
    /str ED /node ED /ypos ED			% stack empty
    % Calculate starting point:
    [ x0 frame_sep sub y0 ypos vstep mul sub ]	% stack: [x y]
    node_dict begin
      aload node ED				% stack: x y
    end
    moveto					% stack empty
    str show
  end
} def

/ASSEM load 0 4 dict put


% move to beginning of line, save the node and plot the text:
% Add a "branch to register" (return) arrow
% stack: ypos node str
/ASSEMRET {
  0 begin
    /str ED /node ED /ypos ED			% stack empty
    % Calculate starting point:
    [ x0 frame_sep sub y0 ypos vstep mul sub ]	% stack: [x y]
    node_dict begin
      aload node ED				% stack: x y
    end
    /y ED /x ED					% stack: empty
    y text_vadjust add /y1 ED
    % Set up for a dotted line:
    [ currentlinewidth currentlinewidth 6 mul] 0 setdash
    () x y1 (???) qqq_indent y1 calculate_line_coords arrow
    % restore solid lines:
    [ ] 0 setdash
    (???) qqq_indent y1 fbox
    x y moveto str show
  end
} def

/ASSEMRET load 0 7 dict put


% Plot the text in the right margin, with a box around it
% and an arrow to the ANNOTATE node
% stack: ypos str
/TEXT {
  0 begin
    /str ED /ypos ED			% stack empty
    % switch to slanted font:
    slanted setfont
    str dup stringwidth pop /width ED	% stack: str
    % calculate the centre of the box:
    xrm width 2 div sub			% stack: str x
    y0 ypos vstep mul sub
    text_vadjust add			% stack: str x y
    dup /y ED				% stack: str x y
    gsave fbox grestore			% stack empty
    % switch back to Courier:
    courier setfont
    % Set up for a dotted line:
    [ currentlinewidth currentlinewidth 6 mul] 0 setdash
    % move to the start point:
    xrm width sub frame_sep sub currentlinewidth sub y
    moveto				% stack empty
    expanded {				% IF expanded THEN
      node_dict begin
	itemB aload pop			% stack: x2 y2
	pop				% stack: x2
	% if x2 is greater than the current x posn,
	% then replace x2 by (current_x_posn - 5):
	dup currentpoint pop		% stack: x2 x2 xcur
	gt {				% IF x2 > xcur
	  pop currentpoint pop 5 sub	%   stack: (xcur - 5)
	} if				% FI
	y				% stack: xc y
	2 copy lineto stroke		% stack: xc y
	2 copy pop 			% stack: xc y xc
	itemB aload pop 		% stack: xc y xc x2 y2
	exch pop			% stack: xc y xc y2
      end
      arrow
    } {					% ELSE
      currentpoint			% stack: x1 y1
      node_dict begin
	itemR aload pop			% stack: x1 y1 x2 y2
      end
      arrow
    } ifelse				% FI
    % restore solid lines:
    [ ] 0 setdash
  end
} def

/TEXT load 0 4 dict put


% draw an up/down arrow from the given nodes with given (left) indent:
% stack: indent from to
/UD {
  0 begin
    node_dict exch get			% stack: indent from [x2 y2]
    aload pop				% stack: indent from x2 y2
    text_vadjust add /y2 ED /x2 ED	% stack: indent from
    node_dict exch get			% stack: indent [x1 y1]
    aload pop				% stack: indent x1 y1
    text_vadjust add /y1 ED /x1 ED	% stack: indent
    /indent ED				% stack empty
    % set up dashed line:
    [ indent indent ] 0 setdash
    % compute the control points:
    /x3 x1 x2 min indent indent_step mul sub def
    % draw the curve:
    x1 y1 moveto
    x3 y1 x3 y2 line_arc arcto pop4
    x3 y2 x2 y2 line_arc arcto pop4
    x2 y2 lineto stroke
    % compute the position of the arrow head, and plot it:
    x3 y1 y2 add 2 div ar_length 2 div	% stack: x3 y4 a_l/2
    y1 y2 lt {
      add
      gsave
	translate		% stack empty
	90 rotate arrow_head
      grestore
    } {
      sub
      gsave
	translate		% stack empty
	-90 rotate arrow_head
      grestore
    } ifelse
  end
} def

/UD load 0 6 dict put
    

% stack: pagelen -->
/STARTPIC {
  % erasepage
  /pagelen ED
  /Courier 5.6 set_text_size
  /Helvetica-Oblique findfont text_size scalefont /slanted ED
  /Courier           findfont text_size scalefont /courier ED
  /vstep 8.5 def
  /indent_step 8.5 def
  % x0 y0 is the position for the first ASSEM line:
  /x0 127.5 def
  /y0 vstep pagelen 2 add mul def
  /xrm 570 def			% x coord of the right margin
  /expanded true def		% TEXT puts the annotation on the next line
				% (which will be left blank on purpose)
  /label () def			% no labels on arrows
  % Clear node dictionary -- holds first, last, itemR, itemB, Nnnn etc.
  /node_dict 500 dict def
  % save first and last nodes in node_dict:
  node_dict begin
    /first [ x0 frame_sep sub y0 vstep  2 mul add ] def
    /last  [ x0 frame_sep sub y0 vstep 94 mul sub ] def
  end
} def

/ENDPIC {
  showpage
} def


% PostScript page header stuff:
/Helvetica-Oblique findfont 10 scalefont /lhead_font ED
/Courier findfont 10 scalefont /chead_font ED
/Times-Roman findfont 10 scalefont /rhead_font ED

% stack: str  -->
/lhead {
  gsave
    x0 y0 vstep 2 mul add moveto
    lhead_font setfont
    show
    x0 y0 vstep 2 mul add vstep 2 div sub moveto
    0.4 setlinewidth
    xrm x0 sub 0 rlineto stroke
  grestore
} def

% stack: str  -->
/chead {
  gsave
    x0 xrm add 2 div y0 vstep 2 mul add moveto
    chead_font setfont
    dup stringwidth pop 2 div neg 0 rmoveto show
  grestore
} def
  

% stack: str  -->
/rhead {
  gsave
    xrm y0 vstep 2 mul add moveto
    rhead_font setfont
    dup stringwidth pop neg 0 rmoveto show
  grestore
} def

%%% High level interface for makepict (call graph plotter)
%%% -- short names to reduce ps file size!

/LINE { calculate_line_coords arrow } def
/ARC  { curve } def
/REC  { rec } def
/N    { fbox } def
/LR   { u_shape } def

%%EndProcSet
%%EndProlog
%%BeginSetup

%%EndSetup
