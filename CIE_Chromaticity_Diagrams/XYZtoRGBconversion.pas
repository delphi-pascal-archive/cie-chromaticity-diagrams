// Delphi implementation of part of John Walker's "Colour Rendering of Spectra"
// from www.fourmilab.ch/documents/specrend.  Most comments from the orignal
// program have been retained.  Additional comments have been added.
//
// efg, November 1998

// References used below:
//
//   [Walker96]  John Walker, "Colour Rendering of Spectra,"
//               wwww.fourmilab.ch/documents/specrend
//
//   [Foley96]   James D. Foley, et al, "Computer Graphics Principles and
//               Practice," Addison-Wesley Publishing Company, 1996.
//
//   [Bruton96]  Dan Bruton
//               Chromaticity Diagram, www.isc.tamu.edu/~astro/color.html

UNIT XYZtoRGBconversion;

{$J-}  // Do not allow constants to be changed below

INTERFACE

  TYPE
    TFloat = SINGLE;   // Use faster singles since conversions called often

    // RGB requires device-specific color specifications to convert the
    // CIE pereptual color into device parameters.

    TColorSystem =
    RECORD
      xRed,   yRed  :  TFloat;
      xGreen, yGreen:  TFloat;
      xBlue,  yBlue :  TFloat;
      xWhite, yWhite:  TFloat               // White Point
    END;


  CONST

    // The following define the x and y coordinates of the phosphors and
    // reference white points of various broadcast systems.

    // [Martindale91] notes that NTSC primaries aren't remotely similar
    // to those used in modern displays.
    //
    // CCIR Report 476-1
    // "Colorimetric Standards in Colour Television"
    // See http://www.igd.fhg.de/icib/tv/ccir/rep_476-1/gen.html
    //
    // CCIR Report 624-4
    // "Characteristics of Television Systems"
    // See http://www.igd.fhg.de/icib/tv/ccir/rep_624/read.html
    NTSCsystem:  TColorSystem =
      (xRed  :  0.67;   yRed  :  0.33;
       xGreen:  0.21;   yGreen:  0.71;
       xBlue :  0.14;   yBlue :  0.08;
       xWhite:  0.310;  yWhite:  0.316);

    // CCIR Report 476-1
    // "Colorimetric Standards in Colour Television"
    // See http://www.igd.fhg.de/icib/tv/ccir/rep_476-1/gen.html
    //
    // CCIR Report 624-4
    // "Characteristics of Television Systems"
    // See http://www.igd.fhg.de/icib/tv/ccir/rep_624/read.html
    PAL_SECAMsystem:  TColorSystem =
      (xRed  :  0.64;   yRed  :  0.33;
       xGreen:  0.29;   yGreen:  0.60;
       xBlue :  0.15;   yBlue :  0.06;
       xWhite:  0.313;  yWhite:  0.329);

    // CCIR Recommendation 709
    // "Basic Parameter Values for the HDTV Standard for the Studio and for
    // International Programme Exchange"
    // See http://www.igd.fhg.de/icib/tv/ccir/rec_709/pictures/page-2.tiff
    //
    // EBU Technical Document 3271 (EBU = European Broadcasting Union)
    // "Interlaced version of the 1250/50 HDTV production standard"
    // http://www.igd.fhg.de/icib/tv/org/ebu/td_3271/pictures/page5.tiff
    EBUsystem:  TColorSystem =
      (xRed  :  0.640;  yRed  :  0.330;
       xGreen:  0.300;  yGreen:  0.600;
       xBlue :  0.150;  yBlue :  0.060;
       xWhite:  0.3127; yWhite:  0.3290);

    // SMPTE 240M (SMPTE = The Society of Motion Picture and Television Engineers)
    // "Signal Parameters -- 1125-line High-Definition Production System"
    // http://www.igd.fhg.de/icib/tv/org/smpte/st_240M-1992/read.html
    SMPTEsystem:  TColorSystem =
      (xRed  :  0.630;  yRed  :  0.340;
       xGreen:  0.310;  yGreen:  0.595;
       xBlue :  0.155;  yBlue :  0.070;
       xWhite:  0.3127; yWhite:  0.3291);       // Illuminant D65

    ShortPersistencePhosphors:  TColorSystem =  // [Foley96, p. 583]
      (xRed  :  0.61;   yRed  :  0.35;
       xGreen:  0.29;   yGreen:  0.59;
       xBlue :  0.15;   yBlue :  0.063;
       xWhite:  0.3101; yWhite:  0.3162);       // Illuminant C

    LongPersistencePhosphors:  TColorSystem =   // [Foley96, p. 583]
      (xRed  :  0.62;   yRed  :  0.33;
       xGreen:  0.21;   yGreen:  0.685;
       xBlue :  0.15;   yBlue :  0.063;
       xWhite:  0.3101; yWhite:  0.3162);       // Illuminant C

    DellPhosphors:  TColorSystem =              // 12 Jan 99 E-mail from Dell
      (xRed  :  0.625;  yRed  :  0.340;         // All Dell monitors except
       xGreen:  0.275;  yGreen:  0.605;         // Nokia 91862
       xBlue :  0.150;  yBlue :  0.065;
       xWhite:  0.3127; yWhite:  0.3291);       // Illuminant D65

  FUNCTION Clamp(CONST value,low,high:  TFloat):  TFloat;

  PROCEDURE XYZtoRGB(CONST ColorSystem:  TColorSystem;
                     CONST xc, yc, zc:  TFloat;
                     VAR   R, G, B:  TFloat);

  FUNCTION ConstrainRGB(CONST ColorSystem:  TColorSystem;
                        VAR x, y, z:  TFloat;
                        VAR R, G, B:  TFloat):  BOOLEAN;

  FUNCTION InsideGamut(CONST R,G,B:  TFloat):  BOOLEAN;


IMPLEMENTATION

  FUNCTION Clamp(CONST value,low,high:  TFloat):  TFloat;
  BEGIN
    IF   value < low
    THEN RESULT := low
    ELSE
      IF   value > high
      THEN RESULT := high
      ELSE RESULT := value
  END {Clamp};


  // =====  XYZtoRGB  ========================================================
  //
  // Given an additive tricolor system, defined by the  CIE  x  and  y
  // chromaticities  of  its  three  primaries (z is derived trivially as
  // 1-x-y, and a desired chromaticity (XC,  YC,  ZC)  in  CIE  space,
  // determine  the  contribution of each primary in a linear combination
  // which  sums  to  the  desired  chromaticity.    If   the   requested
  // chromaticity falls outside the Maxwell triangle (color gamut) formed
  // by the three primaries, one of the  r,  g,  or  b  weights  will  be
  // negative.   Use  InsideGamut to  test  for  a  valid  color  and
  // ConstrainRGB to desaturate an outside-gamut color to the  closest
  // representation within the available gamut.

  PROCEDURE XYZtoRGB(CONST ColorSystem:  TColorSystem;
                     CONST xc, yc, zc:  TFloat;
                     VAR   R, G, B:  TFloat);
    VAR
      d         :  TFloat;
      xr, yr, zr:  TFloat;
      xg, yg, zg:  TFloat;
      xb, yb, zb:  TFloat;
  BEGIN
    // Make these assignments as shorthand for below, and for consistency with
    // [Walker96]
    WITH ColorSystem DO
    BEGIN
      xr := xRed;
      yr := yRed;
      zr := 1.0 - xr - yr;

      xg := xGreen;
      yg := yGreen;
      zg := 1.0 - xg - yg;

      xb := xBlue;
      yb := yBlue;
      zb := 1.0 - xb - yb
    END;

    // See Equation 13.29 in [Foley96, p. 587].  The following solves those
    // equations using Cramer's Rule.

    // Optimization could reduce number of operations here.
    d := xr*yg*zb - xg*yr*zb - xr*yb*zg + xb*yr*zg + xg*yb*zr - xb*yg*zr;

    R := (-xg*yc*zb + xc*yg*zb + xg*yb*zc - xb*yg*zc - xc*yb*zg + xb*yc*zg) / d;

    G := ( xr*yc*zb - xc*yr*zb - xr*yb*zc + xb*yr*zc + xc*yb*zr - xb*yc*zr) / d;

    B := ( xr*yg*zc - xg*yr*zc - xr*yc*zg + xc*yr*zg + xg*yc*zr - xc*yg*zr) / d
  END {XYZtoRGB};


  // =====  InsideGamut  =====================================================
  //
  // Test  whether  a requested color is within the gamut achievable with
  // the primaries of the current colour system.  This amounts  simply  to
  // testing whether all the primary weights are non-negative.

  FUNCTION InsideGamut(CONST R,G,B:  TFloat):  BOOLEAN;
  BEGIN
    RESULT := (R >= 0) AND (G >= 0) AND (B >= 0)
  END {InsideGamut};


  // =====  ConstrainRGB  ====================================================
  //
  // If  the  requested RGB shade contains a negative weight for one of
  // the primaries, it lies outside the color gamut accessible from the
  // given triple of primaries.  Desaturate it by mixing with the white
  // point of the color system so as to reduce the  primary  with the
  // negative weight to zero.  This is equivalent to finding the
  // intersection on the CIE diagram of a line drawn between the white
  // point and the requested color with the edge of the  Maxwell
  // triangle formed by the three primaries.

  FUNCTION ConstrainRGB(CONST ColorSystem:  TColorSystem;
                        VAR x, y, z:  TFloat;
                        VAR R, G, B:  TFloat):  BOOLEAN;
    VAR
      par, wr, wg, wb:  TFloat;

  BEGIN
    //  Is the contribution of one of the primaries negative ?
    IF   InsideGamut(R, G, B)
    THEN RESULT := FALSE   // Color is within gamut
    ELSE BEGIN
      RESULT := TRUE;      // Color modified to fit RGB gamut

      // Yes:  Find the RGB mixing weights of the white point (we
      //       assume the white point is in the gamut!).

      XYZtoRGB(ColorSystem,
               ColorSystem.xWhite,
               ColorSystem.yWhite,
               1.0 - ColorSystem.xWhite - Colorsystem.yWhite,
               wr, wg, wb);

      // Find the primary with negative weight and calculate the parameter
      // of the point on the vector from the white point to the original
      // requested color in RGB space.

      IF  (R < G) AND (R < B)
      THEN par := wr / (wr - R)
      ELSE
        IF  (G < R) AND (G < B)
        THEN par := wg / (wg - G)
        ELSE par := wb / (wb - B);

      // Since XYZ space is a linear transformation of RGB space, we
      // can find the XYZ space coordinates of the point where the
      // edge of the gamut intersects the vector from the white point
      // to the original color by multiplying the parameter in RGB
      // space by the difference vector in XYZ space.

      x := Clamp(ColorSystem.xWhite + par * (x - ColorSystem.xWhite), 0, 1);
      y := Clamp(ColorSystem.yWhite + par * (y - ColorSystem.yWhite), 0, 1);
      z := Clamp(1 - x - y, 0, 1);

      //  Now finally calculate the gamut-constrained RGB weights.
      R := Clamp(wr + par * (R - wr), 0, 1);
      G := Clamp(wg + par * (G - wg), 0, 1);
      B := Clamp(wb + par * (B - wb), 0, 1)
    END
  END {ConstrainRGB};

END.
