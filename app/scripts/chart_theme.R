# Copyright 2019 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

bcstats_chart_theme <-
  theme_bw() +
  theme(
    panel.border = element_rect(colour="white"),
    plot.title = element_text(face="bold"),
    plot.caption = element_text(hjust=0),
    legend.position = c(1,0),
    legend.justification = c(1,0),
    legend.title = element_text(size=12),
    legend.text = element_text(size=11),
    axis.line = element_line(colour="black"),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    axis.text.x = element_text(angle=90)
  )


line_colors <- c("#325A80", "#5091CD", "#00B0F0", "#B7DEE8")
# dark blue   #325A80: 50-90-128
# blue        #5091CD: 80-145-205
# bright blue #00B0F0: 0-176-240
# pale blue   #B7DEE8: 183-222-232