module Example exposing (..)

-- where

import D3.Axis as Axis
import D3.Scale as Scale
import D3.Line as Line
import D3.Format as Format
import Svg
import Svg.Attributes


main =
    let
        data =
            [ ( 5, 0 ), ( 10, 10 ), ( 15, 5 ), ( 20, 15 ) ]

        margin =
            { top = 40, right = 40, bottom = 40, left = 40 }

        width =
            960 - margin.left - margin.right

        height =
            500 - margin.top - margin.bottom

        x =
            Scale.linear ( 5, 20 ) ( 0, width )

        y =
            Scale.linear ( 0, 15 ) ( height, 0 )

        xAxis =
            Axis.axis x
                |> Axis.ticks 3

        yAxis =
            Axis.axis y
                |> Axis.ticks 4
                |> Axis.orient Axis.Right
                |> Axis.tickFormat (toString)

        line =
            Line.line (fst >> Scale.transform x) (snd >> Scale.transform y)

        --|> Line.interpolate Line.Monotone
    in
        Svg.svg
            [ Svg.Attributes.width (Format.px (width + margin.left + margin.right))
            , Svg.Attributes.height (Format.px (height + margin.top + margin.bottom))
            ]
            [ Svg.g
                [ Svg.Attributes.transform
                    (Format.formatString "translate(?,?)"
                        [ margin.left, margin.top ]
                    )
                ]
                [ --svg.append("g")
                  --    .attr("class", "x axis")
                  --    .attr("transform", "translate(0," + height + ")")
                  --    .call(xAxis);
                  Svg.g
                    [ Svg.Attributes.transform
                        (Format.formatString "translate(0,?)"
                            [ height ]
                        )
                    ]
                    [ Axis.render xAxis
                    ]
                  --svg.append("g")
                  --    .attr("class", "y axis")
                  --    .attr("transform", "translate(" + width + ",0)")
                  --    .call(yAxis);
                , Svg.g
                    [ Svg.Attributes.transform
                        (Format.formatString "translate(?,0)"
                            [ width ]
                        )
                    ]
                    [ Axis.render yAxis
                    ]
                  --svg.append("path")
                  --    .attr("class", "line")
                  --    .attr("clip-path", "url(#clip)")
                  --    .attr("d", line);
                , Svg.path
                    [ Svg.Attributes.clipPath "url(#clip)"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.stroke "black"
                    , Svg.Attributes.d (Line.render line data)
                    ]
                    []
                ]
            , Svg.clipPath [ Svg.Attributes.id "clip" ]
                [ Svg.rect
                    [ Svg.Attributes.width (Format.px width)
                    , Svg.Attributes.height (Format.px height)
                    ]
                    []
                ]
            ]



--svg.append("clipPath")
--    .attr("id", "clip")
--  .append("rect")
--    .attr("width", width)
--    .attr("height", height);
