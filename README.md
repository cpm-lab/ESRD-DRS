# End-Stage Renal Disease Dynamic Risk Score (ESRD-DRS)

## Dynamically Predicting ESRD After Development of Diabetes for Millions Across Biobanks

This repository contains the code for analyses in [manuscript name and link here].

-   Veterans Health Administration (VHA) data analysis in the `KDI` folder

    -   Shiny app to explore features extracted in the cohort in [`KDI/ShinyApp`](https://cpm-lab.shinyapps.io/VHAFeatureExtractionExplorer/)

-   NIH All of Us (AoU) data analysis in the `AOU` folder

> Conceptual illustration of how the ESRD Dynamic Risk Score could be displayed within a clinician-facing EHR dashboard, highlighting the risk trajectory across landmark evaluations and the top SHAP-derived contributing factors for individual patients.

<svg viewBox="0 0 720 520" xmlns="http://www.w3.org/2000/svg" font-family="-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif">
  <defs>
    <clipPath id="plotArea"><rect x="80" y="195" width="560" height="175"/></clipPath>
  </defs>

  <!-- Card background -->
  <rect x="10" y="10" width="700" height="500" rx="10" fill="white" stroke="#d3d1c7" stroke-width="0.5"/>

  <!-- Header bar -->
  <line x1="10" y1="70" x2="710" y2="70" stroke="#d3d1c7" stroke-width="0.5"/>

  <!-- Avatar -->
  <circle cx="40" cy="40" r="18" fill="#E6F1FB"/>
  <text x="40" y="44" text-anchor="middle" font-size="11" font-weight="500" fill="#0C447C">JM</text>

  <!-- Patient info -->
  <text x="68" y="35" font-size="14" font-weight="500" fill="#1a1a1a">Patient A</text>
  <text x="68" y="52" font-size="11" fill="#5f5e5a">68 y/o M · DM dx 2016 · eGFR 38 · UACR A3</text>

  <!-- Risk tier badge -->
  <text x="610" y="33" font-size="10" fill="#5f5e5a" text-anchor="end" letter-spacing="0.03em">CURRENT RISK TIER</text>
  <rect x="620" y="24" width="46" height="20" rx="6" fill="#FCEBEB"/>
  <text x="643" y="38" text-anchor="middle" font-size="11" font-weight="500" fill="#791F1F">High</text>

  <!-- Metric cards -->
  <!-- Card 1 -->
  <rect x="25" y="82" width="210" height="52" rx="6" fill="#f5f5f3"/>
  <text x="37" y="100" font-size="10" fill="#5f5e5a">5-yr ESRD risk at LM1</text>
  <text x="37" y="122" font-size="20" font-weight="500" fill="#1a1a1a">0.4%</text>

  <!-- Card 2 -->
  <rect x="250" y="82" width="210" height="52" rx="6" fill="#f5f5f3"/>
  <text x="262" y="100" font-size="10" fill="#5f5e5a">5-yr ESRD risk at LM5</text>
  <text x="262" y="122" font-size="20" font-weight="500" fill="#1a1a1a">2.1%</text>

  <!-- Card 3 -->
  <rect x="475" y="82" width="210" height="52" rx="6" fill="#f5f5f3"/>
  <text x="487" y="100" font-size="10" fill="#5f5e5a">5-yr ESRD risk at LM10</text>
  <text x="487" y="122" font-size="20" font-weight="500" fill="#A32D2D">8.7%</text>

  <!-- Chart title -->
  <text x="25" y="160" font-size="12" font-weight="500" fill="#1a1a1a">ESRD-DRS risk trajectory</text>

  <!-- Chart area -->
  <!-- Y-axis grid lines and labels -->
  <line x1="80" y1="195" x2="640" y2="195" stroke="#e8e8e6" stroke-width="0.5"/>
  <text x="72" y="199" text-anchor="end" font-size="10" fill="#888780">12%</text>

  <line x1="80" y1="224" x2="640" y2="224" stroke="#e8e8e6" stroke-width="0.5"/>
  <text x="72" y="228" text-anchor="end" font-size="10" fill="#888780">10%</text>

  <line x1="80" y1="253" x2="640" y2="253" stroke="#e8e8e6" stroke-width="0.5"/>
  <text x="72" y="257" text-anchor="end" font-size="10" fill="#888780">8%</text>

  <line x1="80" y1="282" x2="640" y2="282" stroke="#e8e8e6" stroke-width="0.5"/>
  <text x="72" y="286" text-anchor="end" font-size="10" fill="#888780">6%</text>

  <line x1="80" y1="311" x2="640" y2="311" stroke="#e8e8e6" stroke-width="0.5"/>
  <text x="72" y="315" text-anchor="end" font-size="10" fill="#888780">4%</text>

  <line x1="80" y1="341" x2="640" y2="341" stroke="#e8e8e6" stroke-width="0.5"/>
  <text x="72" y="345" text-anchor="end" font-size="10" fill="#888780">2%</text>

  <line x1="80" y1="370" x2="640" y2="370" stroke="#e8e8e6" stroke-width="0.5"/>
  <text x="72" y="374" text-anchor="end" font-size="10" fill="#888780">0%</text>

  <!-- Y-axis title -->
  <text x="18" y="283" font-size="10" fill="#888780" transform="rotate(-90 18 283)" text-anchor="middle">5-year predicted ESRD risk</text>

  <!-- X-axis labels -->
  <text x="80" y="388" text-anchor="middle" font-size="11" fill="#888780">Year 1</text>
  <text x="360" y="388" text-anchor="middle" font-size="11" fill="#888780">Year 5</text>
  <text x="640" y="388" text-anchor="middle" font-size="11" fill="#888780">Year 10</text>

  <!-- X-axis title -->
  <text x="360" y="405" text-anchor="middle" font-size="10" fill="#888780">Years since diabetes diagnosis</text>

  <!-- High-risk threshold dashed line at 5% -->
  <line x1="80" y1="297" x2="640" y2="297" stroke="#888780" stroke-width="1.5" stroke-dasharray="6,4"/>
  <text x="545" y="292" font-size="9" fill="#888780">High-risk threshold</text>

  <!-- Patient A line (accelerating): 0.4% → 2.1% → 8.7% -->
  <!-- Y scale: 0% = 370, 12% = 195. Per 1% = 14.58px. -->
  <!-- 0.4% → 370 - 5.8 = 364.2; 2.1% → 370 - 30.6 = 339.4; 8.7% → 370 - 126.9 = 243.1 -->
  <path d="M80,364 Q220,355 360,339 Q500,300 640,243" fill="none" stroke="#E24B4A" stroke-width="2.5" clip-path="url(#plotArea)"/>
  <!-- Fill area -->
  <path d="M80,364 Q220,355 360,339 Q500,300 640,243 L640,370 L80,370 Z" fill="rgba(226,75,74,0.08)" clip-path="url(#plotArea)"/>
  <!-- Data points -->
  <circle cx="80" cy="364" r="5" fill="#E24B4A" stroke="white" stroke-width="2"/>
  <circle cx="360" cy="339" r="5" fill="#E24B4A" stroke="white" stroke-width="2"/>
  <circle cx="640" cy="243" r="5" fill="#E24B4A" stroke="white" stroke-width="2"/>

  <!-- Patient B line (stable): 0.3% → 0.5% → 0.4% -->
  <!-- 0.3% → 365.6; 0.5% → 362.7; 0.4% → 364.2 -->
  <path d="M80,366 Q220,363 360,363 Q500,364 640,364" fill="none" stroke="#1D9E75" stroke-width="2.5" clip-path="url(#plotArea)"/>
  <path d="M80,366 Q220,363 360,363 Q500,364 640,364 L640,370 L80,370 Z" fill="rgba(29,158,117,0.06)" clip-path="url(#plotArea)"/>
  <circle cx="80" cy="366" r="5" fill="#1D9E75" stroke="white" stroke-width="2"/>
  <circle cx="360" cy="363" r="5" fill="#1D9E75" stroke="white" stroke-width="2"/>
  <circle cx="640" cy="364" r="5" fill="#1D9E75" stroke="white" stroke-width="2"/>

  <!-- Legend -->
  <rect x="80" y="413" width="10" height="10" rx="2" fill="#E24B4A"/>
  <text x="95" y="422" font-size="10" fill="#5f5e5a">Patient A (accelerating)</text>
  <rect x="260" y="413" width="10" height="10" rx="2" fill="#1D9E75"/>
  <text x="275" y="422" font-size="10" fill="#5f5e5a">Patient B (stable/declining)</text>
  <line x1="440" y1="418" x2="462" y2="418" stroke="#888780" stroke-width="1.5" stroke-dasharray="4,3"/>
  <text x="468" y="422" font-size="10" fill="#5f5e5a">High-risk threshold</text>

  <!-- Contributing factors section -->
  <line x1="10" y1="440" x2="710" y2="440" stroke="#d3d1c7" stroke-width="0.5"/>
  <rect x="10" y="440" width="700" height="70" rx="0" fill="#f5f5f3"/>
  <rect x="10" y="500" width="700" height="10" rx="0 0 10 10" fill="#f5f5f3"/>

  <text x="25" y="460" font-size="12" font-weight="500" fill="#1a1a1a">Top contributing factors</text>

  <!-- Factor pills -->
  <rect x="25" y="468" width="128" height="22" rx="6" fill="#FCEBEB"/>
  <text x="89" y="483" text-anchor="middle" font-size="11" fill="#791F1F">eGFR 38 (declining)</text>

  <rect x="161" y="468" width="120" height="22" rx="6" fill="#FCEBEB"/>
  <text x="221" y="483" text-anchor="middle" font-size="11" fill="#791F1F">UACR category A3</text>

  <rect x="289" y="468" width="108" height="22" rx="6" fill="#FAEEDA"/>
  <text x="343" y="483" text-anchor="middle" font-size="11" fill="#633806">SBP 148 mmHg</text>

  <rect x="405" y="468" width="56" height="22" rx="6" fill="#E6F1FB"/>
  <text x="433" y="483" text-anchor="middle" font-size="11" fill="#0C447C">Age 68</text>

  <text x="25" y="502" font-size="9" fill="#888780">Risk factors ranked by SHAP contribution to current predicted risk</text>
</svg>
