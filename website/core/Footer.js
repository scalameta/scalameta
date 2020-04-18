const React = require("react");

const siteConfig = require(process.cwd() + "/siteConfig.js");

class Footer extends React.Component {
  render() {
    const currentYear = new Date().getFullYear();
    const {
      copyright,
      colors: { secondaryColor },
    } = this.props.config;
    return (
      <footer
        className="nav-footer"
        id="footer"
        style={{ backgroundColor: secondaryColor }}
      >
        <section className="sitemap">
          {this.props.config.footerIcon && (
            <a href={this.props.config.baseUrl} className="nav-home">
              <img
                src={`${this.props.config.baseUrl}${this.props.config.footerIcon}`}
                alt={this.props.config.title}
                width="66"
                height="58"
              />
            </a>
          )}
          <div>
            <h5>Scalameta Docs</h5>
            <a href={` ${this.props.config.baseUrl}docs/trees/guide.html`}>
              Trees Guide
            </a>
            <a
              href={` ${this.props.config.baseUrl}docs/trees/quasiquotes.html`}
            >
              Quasiquotes
            </a>
            <a
              href={` ${this.props.config.baseUrl}docs/semanticdb/specification.html`}
            >
              SemanticDB
            </a>
          </div>
          <div>
            <h5>Scalameta Projects</h5>
            <a href="https://scalameta.org/metals">Metals: language server</a>
            <a href="https://scalameta.org/scalafmt">
              Scalafmt: code formatter
            </a>
            <a href="https://scalacenter.github.io/scalafix">
              Scalafix: linting and refactoring tool
            </a>
            <a href="https://scalameta.org/munit">MUnit: testing library</a>
            <a href="https://scalameta.org/mdoc">MDoc: documentation tool</a>
            <a href="https://github.com/scalameta/metabrowse">
              Metabrowse: online code browser
            </a>
          </div>
          <div>
            <h5>Community</h5>
            <a href={siteConfig.repoUrl} target="_blank">
              GitHub
            </a>
            <a href="https://github.com/scalameta/scalameta" target="_blank">
              <img src="https://img.shields.io/github/stars/scalameta/metals.svg?color=%23087e8b&label=stars&logo=github&style=social" />
            </a>
            <a href="https://discord.gg/RFpSVth" target="_blank">
              <img src="https://img.shields.io/discord/632642981228314653?logo=discord&style=social" />
            </a>
            <a href="https://gitter.im/scalameta/scalameta" target="_blank">
              <img src="https://img.shields.io/gitter/room/scalameta/scalameta.svg?logo=gitter&style=social" />
            </a>
            <a href="https://twitter.com/scalameta" target="_blank">
              <img src="https://img.shields.io/twitter/follow/scalameta.svg?logo=twitter&style=social" />
            </a>
          </div>
        </section>
        <section className="copyright">{copyright}</section>
      </footer>
    );
  }
}

module.exports = Footer;
