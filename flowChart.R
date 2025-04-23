# Install if needed

load_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}

load_package("DiagrammeRsvg")
load_package("DiagrammeR")
load_package("rsvg")



# Create the graph and save as object
graph <- grViz("
digraph prisma_flow {
  graph [layout = dot, rankdir = TB]
  node [shape = box, style = rounded, fontsize = 10, fontname = Helvetica]

  A [label = 'RCTs (7) identifed from\\n(n = 213)\\nCentral = 41\\nEmbase = 131\\nMedline = 41
  \\nfrom Open Heart2022;9:e001887. \\ndoi:10.1136/
openhrt-2021-001887\\n until 2/2021']
  B [label = 'Additional records identified\\nfrom 2/2021\\n(n=90)']
  C [label = 'Records screened\\n(n = 91)']
  D [label = 'Records excluded\\n(n = 83)']
  E [label = 'Full-text articles assessed\\nfor eligibility\\n(n = 8)']
  F [label = 'Studies included in\\nquantitative synthesis\\n(meta-analysis)\\n(n = 8)']

  A -> C
  B -> C
  C -> D
  C -> E
  E -> F
}
")

# Convert to SVG and then to PNG and PDF
svg <- export_svg(graph)
charToRaw(svg) %>% rsvg_png("output/prisma_flowchart.png", width = 1200)
charToRaw(svg) %>% rsvg_pdf("output/prisma_flowchart.pdf")

