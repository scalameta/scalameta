import type { Config } from "@docusaurus/types";

const repoUrl = "https://github.com/scalameta/scalameta";

export default {
  title: "Scalameta",
  tagline: "Library to read, analyze, transform and generate Scala programs",
  url: "https://scalameta.org",
  baseUrl: "/",
  organizationName: "scalameta",
  // org-pages repo; GitHub serves it from `master` (CNAME scalameta.org).
  projectName: "scalameta.github.io",
  deploymentBranch: "master",
  trailingSlash: false,
  favicon: "img/favicon.ico",
  customFields: { repoUrl },
  onBrokenLinks: "log",
  // .md files (all our docs) parse as CommonMark, so raw HTML in the docs
  // passes through untouched instead of hitting MDX's stricter JSX rules.
  markdown: { format: "detect", hooks: { onBrokenMarkdownLinks: "log" } },
  presets: [
    [
      "@docusaurus/preset-classic",
      {
        docs: {
          // mdoc writes generated markdown here (see build.sbt docs project).
          path: "../website/target/docs",
          sidebarPath: "../website/sidebars.json",
          // No editUrl: docs come from two repos (scalameta + tutorial).
        },
        theme: { customCss: "./src/css/customTheme.css" },
      },
    ],
  ],
  plugins: ["@easyops-cn/docusaurus-search-local"],
  themeConfig: {
    colorMode: { respectPrefersColorScheme: true },
    image: "img/scalameta.png",
    navbar: {
      title: "Scalameta",
      logo: { alt: "Scalameta", src: "img/scalameta.png" },
      items: [
        { to: "docs/trees/guide", label: "Trees", position: "left" },
        { to: "docs/semanticdb/guide", label: "SemanticDB", position: "left" },
        {
          href: repoUrl,
          position: "right",
          className: "header-github-link",
          "aria-label": "GitHub repository",
        },
      ],
    },
    footer: {
      style: "dark",
      links: [
        {
          title: "Scalameta Docs",
          items: [
            { label: "Trees Guide", to: "docs/trees/guide" },
            { label: "Quasiquotes", to: "docs/trees/quasiquotes" },
            { label: "SemanticDB", to: "docs/semanticdb/specification" },
          ],
        },
        {
          title: "Scalameta Projects",
          items: [
            { label: "Metals: language server", href: "https://scalameta.org/metals" },
            { label: "Scalafmt: code formatter", href: "https://scalameta.org/scalafmt" },
            { label: "Scalafix: linting and refactoring", href: "https://scalacenter.github.io/scalafix" },
            { label: "MUnit: testing library", href: "https://scalameta.org/munit" },
            { label: "MDoc: documentation tool", href: "https://scalameta.org/mdoc" },
            { label: "Metabrowse: online code browser", href: "https://github.com/scalameta/metabrowse" },
          ],
        },
        {
          title: "Community",
          items: [
            { label: "GitHub", href: repoUrl },
            { label: "Discord", href: "https://discord.gg/RFpSVth" },
            { label: "Twitter", href: "https://twitter.com/scalameta" },
          ],
        },
      ],
      copyright: `Copyright © ${new Date().getFullYear()} Scalameta`,
    },
    prism: {
      // Order matters: Prism loads these without resolving deps; scala extends java.
      additionalLanguages: ["java", "scala"],
    },
  },
} satisfies Config;
