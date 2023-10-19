import React from 'react';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Link from '@docusaurus/Link';

export default function ScaladocLink({children, path}) {
  const {siteConfig: {baseUrl}} = useDocusaurusContext();
  return (
    <Link to={`${baseUrl}scaladoc/current/${path}`} target="_blank" >{children}</Link>
  )
}