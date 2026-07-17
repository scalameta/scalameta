import React from "react";
import Link from "@docusaurus/Link";
import useDocusaurusContext from "@docusaurus/useDocusaurusContext";
import Layout from "@theme/Layout";

const Features = () => {
  const features = [
    {
      title: "Syntactic API",
      content:
        "Parse, transform and prettyprint Scala source code with high-level " +
        "APIs and out-of-the-box IDE support.",
      image: "img/syntactic-api.png",
      imageAlign: "right",
    },
    {
      title: "Semantic API",
      content: "Build developer tools that understand Scala symbols and types.",
      image: "img/semantic-api.png",
      imageAlign: "left",
    },
    {
      title: "Industrial adoption",
      content:
        "Scalameta libraries get downloaded from over 100,000 unique IP " +
        "addresses each month, Scalameta quasiquotes have built-in support in " +
        "IntelliJ, and Scalameta is actively developed and maintained by " +
        "engineers at Twitter, the Scala Center and members from the community.",
      image: "img/established.png",
      imageAlign: "right",
    },
  ];
  return (
    <div>
      {features.map((feature) => (
        <div className="hero" key={feature.title}>
          <div
            className={`container scalameta-feature ${
              feature.imageAlign === "right" ? "" : "scalameta-feature--reverse"
            }`}
          >
            <div className="padding--md">
              <h2 className="hero__subtitle">{feature.title}</h2>
              <p>{feature.content}</p>
            </div>
            <div className="padding--md">
              <img src={feature.image} alt={feature.title} />
            </div>
          </div>
        </div>
      ))}
    </div>
  );
};

const Index = () => {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout title={siteConfig.title} description={siteConfig.tagline}>
      <div className="hero text--center">
        <div className="container">
          <div className="padding-vert--md">
            <h1 className="hero__title">{siteConfig.title}</h1>
            <p className="hero__subtitle">{siteConfig.tagline}</p>
          </div>
          <Link
            to="docs/trees/guide"
            className="button button--lg button--outline button--primary margin--sm"
          >
            Get Started
          </Link>
        </div>
      </div>
      <div className="container margin-vert--lg">
        <Features />
      </div>
    </Layout>
  );
};

export default Index;
