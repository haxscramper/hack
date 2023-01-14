function wrap_target(name, body)
    target(name)
    body()
    target_end()
end

function wrap_option(name, body)
    option(name)
    body()
    option_end()
end

function wrap_rule(name, body)
    rule(name)
    body()
    rule_end()
end

wrap_option(
    "grammar_name",
    function()
        set_description("Antlr name of the grammar")
    end
)

function on_g4_build(target, batchcmds, sourcefile_antlr4, opt)
    local sourcedir_cx =
        path.join(
        target:autogendir(),
        "rules",
        "antlr4",
        path.basename(sourcefile_antlr4)
    )

    -- target:add_files(sourcedir_cx .. "/*.cpp")

    -- add commands
    batchcmds:show_progress(
        opt.progress,
        "${color.build.object}compiling.antlr4 %s",
        sourcefile_antlr4
    )
    batchcmds:mkdir(path.directory(sourcedir_cx))

    batchcmds:vrunv(
        "antlr4",
        {
            "-Dlanguage=Cpp",
            "-visitor",
            "-o",
            path(sourcedir_cx),
            path(sourcefile_antlr4)
        }
    )

    for _, file in ipairs(os.files(sourcedir_cx .. "/*.cpp")) do
        local objectfile = target:objectfile(file)
        table.insert(target:objectfiles(), objectfile)
        batchcmds:compile(file, objectfile)
    end

    -- add deps
    batchcmds:add_depfiles(sourcefile_antlr4)
    -- batchcmds:set_depmtime(os.mtime(objectfile))
    -- batchcmds:set_depcache(target:dependfile(objectfile))
end

wrap_rule(
    "antlr4",
    function()
        set_extensions(".g4")
        on_buildcmd_file(on_g4_build)
    end
)

if has_config("grammar_name") then
    local name = get_config("grammar_name")
    add_requires("termcolor")
    set_config("buildir", name .. "_build")

    wrap_target(
        name .. "_build",
        function()
            set_kind("binary")
            add_links("antlr4-runtime")
            add_rules("antlr4")
            add_files(name .. ".g4")
            add_files(name .. "_user.cpp")
            add_packages("termcolor")
            add_includedirs("/usr/include/antlr4-runtime")
            add_options("grammar_name")

            set_targetdir(".")
            set_extension(".bin")
        end
    )
end
