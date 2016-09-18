\begin{figure}
    \centering
    \begin{tikzpicture}[scale=5]
    
    %boundary of polygon
    \path[draw] (1,0) -- (1/2, 0.866) -- (-1/2, 0.866) -- (-1, 0);

    %graph edges
    \begin{scope}[very thick,decoration={
    markings,
    mark=at position 0.5 with {\arrow{>}}}
    ] 

        \draw[postaction={decorate}]  (0.1, 0.3) -- (0.75, 0.433) node[pos=.5,sloped,above]{$h$};
        % old g
        % \draw[postaction={decorate}]  (0.1, 0.3) -- (0, 0.866) node[pos=.5,left]{$g$};
        \draw[postaction={decorate}]  (-0.75, 0.433) -- (0, 0.2) node[pos=.5,sloped,above]{$h$};
        \draw[postaction={decorate}]  (0, 0.2) -- (0, 0) node[pos=.5,left]{$hg^{-1}h$};
        \draw (0, 0.2) -- (0.1, 0.3);

        % new g
        \draw[postaction={decorate}]  (0.1, 0.3) -- (0.6, 0.6928)
        node[pos=.5,sloped,above]{$g$};
        \draw[postaction={decorate}]  (-0.6, 0.6928) -- (0, 0.866)
        node[pos=.5,sloped,below]{$g$};

        \draw (-0.15, 0.8227) -- (0.05, 0.25);

    \end{scope}
    \end{tikzpicture}
    \caption{First type of Dehn twist}
    \label{fig:tikzTwist1_3}
\end{figure}


