library(DiagrammeR)

grViz("
digraph boxes_and_circles {

  # a 'graph' statement
  graph [overlap = true, fontsize = 10]


  node [shape = none,
        fontname = Helvetica,
        fontsize = 8]
  a0[label = 'Starting data'];
  a1[label = 'rolling_stat.R\\l - Round \\l - Completeness \\l'];
  a2[label = 'daily_stat.R\\l - Round \\l - Exceedances \\l - Completeness \\l'];
  a3[label = 'yearly_stat.R\\l - Exclude days \\l - Round \\l - Exceedances \\l - Completeness \\l'];
  a4[label = 'rolling_stat.R\\l - Round \\l - Completeness\\l'];
  a5[label = 'caaqs.R\\l - CAAQs cutoffs\\l'];

  node [shape = box,
        fontname = Helvetica,
        style = filled,
        fillcolor = white]
  pm0[label = 'Raw PM 2.5 data', group = g1];

  subgraph cluster_pm_wrap {
    style = filled;
    color = PaleGreen;
    pm1[label = 'pm_daily_avg()\\n (round 1 digit)', group = g1];
    pm2[label = 'pm_yearly_98()\\n (round 1 digit)\\n (use exceedances)', group = g2]; 
    pm3[label = 'pm_yearly_avg()\\n (round 1 digit)', group = g3];
    pm4[label = 'pm_three_yr_avg()\\n (round 0 digits 98th)\\n (round 1 digit annual)', group = g1];
    pm5[label = 'pm_24h_caaqs()', group = g2];
    pm6[label = 'pm_ann_caaqs()', group = g3];
  }

  # several 'edge' statements
  pm0 -> pm1[minlen = 2]
  pm1 -> pm2
  pm1 -> pm3
  pm2 -> pm4
  pm3 -> pm4
  pm4 -> pm5
  pm4 -> pm6

  o0[label = 'Raw O3 data'];
  subgraph cluster_o3_wrap {
    style = filled;
    color = PaleGreen;
    o1[label = 'o3_rolling_8hr_avg()\\n (round 1 digit)'];
    o2[label = 'o3_daily_max()\\n (round 1 digit)\\n (use exceedances)']; 
    o3[label = 'o3_ann_4th_highest()\\n (round 1 digit)\\n (use exceedances)'];
    o4[label = 'o3_three_yr_avg()\\n (round 0 digits)'];
    o5[label = 'o3_caaqs()'];
  }

  edge[]
  o0 -> o1
  o1 -> o2
  o2 -> o3
  o3 -> o4
  o4 -> o5

  so0[label = 'Raw SO2 data'];
  subgraph cluster_so21_wrap {
    style = filled;
    color = PaleGreen;
    so1[label = 'so2_daily_max()\\n (round 1 digit)\\n (use exceedances)'];
    so2[label = 'so2_yearly_99()\\n (round 1 digit)\\n (use exceedances)']; 
    so3[label = 'so2_three_yr_avg()\\n (round 1 digit)'];
    so4[label = 'so2_3yr_caaqs()'];
  }

  subgraph cluster_so22_wrap {
    style = filled;
    color = PaleGreen;
    so5[label = 'so2_avg_hourly_by_year()\\n (round 1 digit)\\n (use exceedances)'];
    so6[label = 'so2_1yr_caaqs()'];
  }

  so0 -> so1[minlen = 2]
  so1 -> so2
  so2 -> so3
  so3 -> so4
  
  so0 -> so5[minlen = 3]
  so5 -> so6[minlen = 2]

  no0[label = 'Raw NO2 data'];
  subgraph cluster_no21_wrap {
    style = filled;
    color = PaleGreen;
    no1[label = 'no2_daily_max()\\n (round 1 digit)'];
    no2[label = 'no2_yearly_98()\\n (round 1 digit)']; 
    no3[label = 'no2_three_yr_avg()\\n (round 1 digit)'];
    no4[label = 'no2_3yr_caaqs()'];
  }

  subgraph cluster_no22_wrap {
    style = filled;
    color = PaleGreen;
    no5[label = 'no2_avg_hourly_by_year()\\n (round 1 digit)'];
    no6[label = 'no2_1yr_caaqs()'];
  }

  no0 -> no1[minlen = 2]
  no1 -> no2
  no2 -> no3
  no3 -> no4
  
  no0 -> no5[minlen = 3]
  no5 -> no6[minlen = 2]
  
  # {rank = same; a0, pm0, o0, so0, no0}
  # {rank = same; a2, pm1, o2, so1, so5, no1, no5}
  # {rank = same; a3, pm2, pm3, o3, so2, no2}
  # {rank = same; a4, pm4, o4, so3, no3}
  # {rank = same; a5, pm5, pm6, o5, so4, so6, no4, no6}

  edge[style=invis]

  pm1 -> pm4
  pm2 -> pm5
  pm3 -> pm6
  a0 -> a1
  a1 -> a2
  a2 -> a3
  a3 -> a4
  a4 -> a5
}
")
