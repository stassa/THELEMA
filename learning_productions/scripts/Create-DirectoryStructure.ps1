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
        $DataSetsDirectory = ".\languages"
        $LogsDirectory = ".\logs"
        $OutputDirectory = ".\output"

        $Directories = $CorpusDirectory, $DataSetsDirectory, $LogsDirectory, $OutputDirectory

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

