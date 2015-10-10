<# Script to set up output directories #>
function Create-DirectoryStructure {
    Param(
        [Parameter(Mandatory=$false
                  ,HelpMessage = "Whether to force overwrite of existing directories."
        )]
        [Switch]
        $Overwrite=$false
    )

    process {
        $CorpusDirectory = ".\corpus"
        $LanguageDirectory = ".\language"
        $LogsDirectory = ".\logs"
        $OutputDirectory = ".\output"
        $VisualisationsDirectory = ".\visualisations"

        $Directories = $CorpusDirectory, $LanguageDirectory, $LogsDirectory, $OutputDirectory, $VisualisationsDirectory

        foreach ($Directory in $Directories) {

            if ((Test-Path $Directory) -and -not $Overwrite) {
                Write-Host "Directory " $Directory " exists; skipping." -BackgroundColor Red -ForegroundColor Yellow
            }
            else {
                    New-Item $Directory -ItemType directory -Force
            }
        }

    }

}

