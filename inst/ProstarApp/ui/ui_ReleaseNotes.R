tabPanel("Release notes",
    value = "ReleaseNotesTab",
    shinyBS::bsCollapse(
        id = "collapseFormerReleases",
        open = "Current release",
        multiple = TRUE,
        shinyBS::bsCollapsePanel("Current release",
                                 mod_insert_md_ui("versionNotes_MD"),
            style = "info"
        ),
        shinyBS::bsCollapsePanel("Former releases",
                                 mod_insert_md_ui("formerReleases_MD"),
            style = "info"
        )
    ),
)
